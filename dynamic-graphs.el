;;; dynamic-graphs.el --- Manipulation with graphviz graphs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020  Tomas Zellerin
;;
;; Author: Tomas Zellerin <tomas@zellerin.cz>
;; Keywords: tools
;; Package-version: 0.93
;; URL: https://github.com/zellerin/dynamic-graphs
;; Package-Requires: ((emacs "26.1"))
;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Make dynamic graphs: take a graph (as defined for graphviz), apply
;; some filters, and display it as an image.  The graph can be either
;; a function that inserts the graph (and is called for each
;; redisplay), or a buffer that can change.  An imap file is created
;; in addition to the image so that clicks on image can be related to
;; individual nodes (TODO: only for rectangles so far)
;;
;; The filters can apply both enhancing operations (add colors, ...)
;; and more complicated operations coded in gvpr.  As a special case
;; there is a filter that removes all nodes that are more distant than
;; a parameter from a root node.
;;
;; The image is displayed with a specialized minor mode.
;; Predefined key bindings on the displayed image in this mode include:
;; - e (dynamic-graphs-set-engine) change grahviz engine (dot, circo, ...)
;; - c (dynamic-graphs-remove-cycles) change whether cycles are removed
;; - 1-9 (dynamic-graphs-zoom-by-key) set maximum displayed distance from a root node
;; - mouse-1 (dynamic-graphs-shift-focus-or-follow-link) shift root
;;   node or follow link defined in imap file - that is, in URL
;;   attribute of the node.  Link is followed by customizable
;;   function, by default `browse-url' - but
;;   `org-link-open-from-string' might be more useful.  See docstring
;;   for details.
;;
;; Example:
;;
;; (dynamic-graphs-display-graph "test" nil
;;		   (lambda ()
;;		     (insert "digraph Gr {A->B B->C C->A A->E->F->G}"))
;;		   '(2 remove-cycles "N {style=\"filled\",fillcolor=\"lightgreen\"}"
;;                   node-refs boxize))
;;
;;; Code:
;;
;;; Customizable variable
(defcustom dynamic-graphs-filters '(3 default remove-cycles)
  "Default filter for dynamic-graphs.

This should be list of filters.

 Each filter can be:
- a string that denotes either name of a gvpr file to be applied or
  direct gvpr transformation,
- an integer that denotes that only nodes with distance from root less
  or equal to the number should be kept,
- symbol `remove-cycles' that causes cycles removal,
- symbol `debug' that does not filter the graph, but displays in the
  `*messages*' buffer current graph.

The variable is set buffer-local in the image buffers so that it can
be changed dynamically."
  :group 'dynamic-graphs
  :type '(repeat
	  (choice (integer :tag "Maximum distance from root to keep")
		  (string :tag "gvpr code to apply")
		  (file :must-match t :tag "gvpr source file to apply")
		  (const :tag "Remove cycles in graph" remove-cycles)
		  (symbol :tag "Reference to dynamic-graphs-transformations")
		  (const debug :tag "Dump transformed graph to *messages*"))))

(put 'dynamic-graphs-filters 'permanent-local t)

(defvar dynamic-graphs-engines
  '((?d "dot" "directed graphs")
    (?c "circo" "undirected graphs")
    (?n "neato" "radial layouts")
    (?t "twopi" "circular layout")
    (?f "fdp" "undirected graphs")
    (?s "sfdp" "large undirected graphs")
    (?o "osage" "squarified tree map")
    (?p "patchwork" "array based layouts"))
  "List of available Graphviz programs with intended usage.")

(defcustom dynamic-graphs-cmd "twopi"
  "Command to create final image."
  :group 'dynamic-graphs
  :type `(choice ,@(mapcar (lambda (a)
			     `(const  :tag ,(format "%s - %s" (cadr a) (caddr a)) ,(cadr a)))
			   dynamic-graphs-engines)))

(put 'dynamic-graphs-cmd 'permanent-local t)

(defcustom dynamic-graphs-image-directory (file-truename temporary-file-directory)
  "Directory for the created images."
  :group 'dynamic-graphs
  :type 'directory)

(defcustom dynamic-graphs-follow-link-fn 'browse-url
  "Function to follow links in the graphs.

I found `org-link-open-from-string' more useful than `browse-url', but if
I set it as default, I would have to make org mode a dependency."
  :type '(choice
	  (const browse-url)
	  (const org-link-open-from-string)
	  function)
  :group 'dynamic-graph)

(defvar dynamic-graphs-make-graph-fn
  (lambda () (error "Source of graph unknown, cant rebuild after change"))
  "Function that creates the displayed graph.")

(make-variable-buffer-local 'dynamic-graphs-make-graph-fn)
(put 'dynamic-graphs-filters 'permanent-local t)

(defvar-local dynamic-graphs-root nil
  "Root node for dijkstra algorithm if set.

The variable is set buffer-local in the image buffers so that it can
be changed dynamically.")

(defcustom dynamic-graphs-transformations
  '((boxize . "N {shape=\"box\"}")
    (node-refs . "N[!URL] {URL=sprintf(\"id:%s\", $.name)}")
    (default . "BEG_G{overlap=\"false\", fontname=\"Courier\"}
N[dist && dist==0.0]{style=\"filled\",fillcolor=\"yellow\",fontsize=\"22\"}"))
  "Named predefined transformations gvdr snippets or sources.

The names can be used in the `dynamic-graphs-filters' to make it more
manageable.

Predefined cases include:
- boxize :: sets nodes shape to box.  That is the only imap file the
  code can handle at the moment,
- default :: sample simple transformation used in the default filter.
  User is expected to customize it based on the preferences, or change
  it to a style file.
- node-ref :: add URL to each node based on its name.  This is needed
  for moving root around."
  :group 'dynamic-graphs
  :type '(alist :key-type symbol :value-type string))

(defcustom dynamic-graphs-ignore-ids ()
  "IDs that are ignored in graphs.

Work in progress, do not expect it to work now.

The variable is set buffer-local in the image buffers so that it can
be changed dynamically."
  :group 'dynamic-graphs
  :type '(repeat string))

;;; Helper functions
(defun dynamic-graphs-get-scale (image)
  "Get scale of the IMAGE."
  (let ((size (and image (image-size image t))))
    (if (and image
	     (eq (car image) 'image)
	     ;; 27.1 support
	     (or (and (fboundp 'image-transforms-p) (image-transforms-p))
		 (eq (plist-get (cdr image) :type) 'imagemagick)))
	;; see comment i n image.c for compute_image_size: -1 x -1 is
	;; native
	(let* ((full-image (create-image (plist-get (cdr image) :file)
					 nil nil :width -1 :height -1))
	       (full-size
		(progn (image-flush full-image)
		       (image-size full-image t))))
	  (/ (float (car full-size)) (car size)))
      1.0)))

(defun dynamic-graphs-cmd (name &rest pars)
  "Apply command NAME with PARS as a filter on the current buffer.

Throw error if it failed."
  (let ((before (buffer-string))
	(res (apply #'call-process-region (point-min)
		    (point-max) name pars)))
    (unless (or (zerop res) (and (equal name "acyclic") (< res 255)))
      (error (format "failed %s: %s->%s" name before (buffer-string))))))

(defun dynamic-graphs-apply-filters (filters)
  "Apply FILTERS on current buffer.

 See `dynamic-graphs-filters' for the syntax."
  (let ((root dynamic-graphs-root))
    (dolist (filter (or filters dynamic-graphs-filters))
      (when (symbolp filter)
	(setq filter
	      (or (cdr (assoc filter dynamic-graphs-transformations))
		  filter)))
      (cond
       ((and (stringp filter) (file-exists-p (expand-file-name filter)))
	(dynamic-graphs-cmd "gvpr" t t nil "-c" "-qf" filter))
       ((and (stringp filter))
	(dynamic-graphs-cmd "gvpr" t t nil "-c" "-q" filter))
       ((integerp filter)
	(when root
	  (dynamic-graphs-cmd "dijkstra" t t nil root)
	  (dynamic-graphs-cmd "gvpr" t t nil "-c" "-a" (format "%d.0" filter)
		"-q" "BEGIN{float maxdist; sscanf(ARGV[0], \"%f\", &maxdist)}
 N[!dist || dist >= maxdist] {delete(root, $)}")))
       ((eq filter 'remove-cycles)
	(dynamic-graphs-cmd "acyclic" t t)
	(dynamic-graphs-cmd "tred" t t))
       ((eq filter 'debug)
	(message "Filters debug: %s" (buffer-string)))
       ((and (consp filter)
	     (eq (car filter) 'ignore))
	(delete-matching-lines (regexp-opt (cdr filter))
			       (point-min) (point-max)))
       (t (error "Unknown transformation %s" filter)))))  )

(defun dynamic-graphs-create-outputs (suffixes &optional base-file-name root make-graph-fn filters)
  "Create files of types SUFFIXES.

The files are created in the `dynamic-graphs-image-directory'
directory named by the `BASE-FILE-NAME'.

The `MAKE-GRAPH-FN' inserts the original graph into the buffer.  The
graph is modified as specified by the list `FILTERS'.  See
`dynamic-graphs-filters' for the syntax.

The `ROOT', if not null, indicates the root node for cutting off far
nodes.

Finally, process the graph with variable `dynamic-graphs-cmd' to create
outputs with each of SUFFIXES type."
  (let ((cmd dynamic-graphs-cmd)
	(base-file-name (or base-file-name (file-name-base (buffer-file-name))))
	(root (or root dynamic-graphs-root))
	(make-graph-fn (or make-graph-fn dynamic-graphs-make-graph-fn))
	(filters (or filters dynamic-graphs-filters)))
    (with-temp-buffer
      (funcall make-graph-fn)
      (setq dynamic-graphs-root root)
      (dynamic-graphs-apply-filters filters)
      (dolist (type suffixes)
	(dynamic-graphs-cmd cmd
	      nil nil nil "-o" (concat dynamic-graphs-image-directory "/" base-file-name "." type) "-T" type)))))

(defun dynamic-graphs-rebuild-graph (base-file-name root make-graph-fn &optional filters)
  "Create png and imap files.

The files are created in the `dynamic-graphs-image-directory'
directory named by the `BASE-FILE-NAME'.

The `MAKE-GRAPH-FN' inserts the original graph into the buffer.  The
graph is modified as specified by the list `FILTERS'.  See
`dynamic-graphs-filters' for the syntax.

The `ROOT', if not null, indicates the root node for cutting off far
nodes.

Finally, process the graph with variable `dynamic-graphs-cmd' to
create image and imap file from the final graph.

Return the graph as the string (mainly for debugging purposes)."
  (dynamic-graphs-create-outputs '("png" "cmapx") base-file-name root make-graph-fn filters))

(defun dynamic-graphs-save-gv ()
  "Save current image in dot format as .gv file."
  (interactive)
  (dynamic-graphs-create-outputs '("gv")))

(defun dynamic-graphs-save-pdf ()
  "Save current image as .pdf file."
  (interactive)
  (dynamic-graphs-create-outputs '("pdf")))

(defun dynamic-graphs-rebuild-and-display (&optional base-file-name root make-graph-fn filters)
  "Redisplay the graph in the current buffer.

  Specifically set local values of some global parameters and run
  `dynamic-graphs-rebuild-graph' with appropriate arguments.

  Display the resulting png file or, when there is already a buffer
  with the file, redisplay.

Arguments `BASE-FILE-NAME', `ROOT', `MAKE-GRAPH-FN' and `FILTERS' are as
in `REBUILD-GRAPH'"
  (dynamic-graphs-rebuild-graph base-file-name root make-graph-fn filters)
  ;; display it
  (let ((old-image-buffer (get-file-buffer (concat dynamic-graphs-image-directory base-file-name ".png"))))
    (if (null old-image-buffer)
	(find-file (concat dynamic-graphs-image-directory base-file-name ".png"))
      (switch-to-buffer old-image-buffer)
      (setq-local revert-buffer-function 'revert-buffer--default)
      (revert-buffer t t)))
  (setq-local dynamic-graphs-filters filters)
  (setq-local dynamic-graphs-root root)
  (setq-local dynamic-graphs-make-graph-fn make-graph-fn)
  (dynamic-graphs-graph-mode t))

;;;###autoload
(defun dynamic-graphs-display-graph (&optional base-file-name root make-graph-fn filters)
  "Display graph image and put it dynamic-graphs-mode on.

This is a shortcut for `dynamic-graphs-rebuild-graph' with defaulting
parameters.

All parameters - BASE-FILE-NAME ROOT MAKE-GRAPH-FN and FILTERS - are
optional with sensible defaults."
  (dynamic-graphs-rebuild-and-display (or base-file-name (file-name-base))
			(or root dynamic-graphs-root)
			(or make-graph-fn dynamic-graphs-make-graph-fn)
			(or filters dynamic-graphs-filters)))

;;;###autoload
(defun dynamic-graphs-display-graph-buffer (root filters)
  "Make a dynamic graph from a graphviz buffer.

There is no default ROOT
node by default, and `dynamic-graphs-filters' - either default value or a
buffer-local if set, are used as default FILTERS when called
interactively."
  (interactive (list nil dynamic-graphs-filters))
  (let* ((buffer (current-buffer))
	 (buffer-directory (when (buffer-file-name buffer)
			     (expand-file-name (file-name-directory (buffer-file-name buffer))))))
    (dynamic-graphs-rebuild-and-display (if (buffer-file-name) (file-name-base) (read-string "Graph name: "))
			  root
			  (lambda ()
			    (insert-buffer-substring buffer)
			    (when buffer-directory
			      (setq default-directory buffer-directory)))
			  filters)))

(defun dynamic-graphs--get-coords ()
  "Read coordinates from image so that they can be used in a map definition."
  (interactive)
  (let ((e (read-event "Click somewhere: ")))
    (let* ((scale (dynamic-graphs-get-scale (get-char-property (point-min) 'display)))
	   (pos (posn-x-y (event-start e)))
	   (x (* (car pos) scale))
	   (y (* (cdr pos) scale)))
      (message "%s %s" x y))))

;;; Mouse handlers (expect imap/cmapx file in place with proper structure)
(defun dynamic-graphs-get-rects-sexp (sexp x y)
  "Get reference related to the screen point X Y from the SEXP that describes node links."
  ;; SEXP is  (map ((id . XXX)) (area ((...)))...
  (with-temp-buffer
    (seq-some
     (lambda (item)
       (when (eq (car item) 'area)
	 (let-alist (cadr item)
	   (let ((coords (mapcar #'string-to-number (split-string  .coords ","))))
	     (if (and
		  (eq .shape "rect")
		  (> (nth 2 coords) x (nth 0 coords))
		  (> (nth 3 coords) y  (nth 1 coords)))
		 (cadr item))))))
     sexp)))

(defun dynamic-graphs-get-rects-imap (file x y)
  "Get reference related to the screen point X Y from the imap FILE.

To be used for existing imap files; note that cmapx format is
preferred and generated now."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char 1)
      (let (res)
	(while (and (null res)
		    (re-search-forward (rx bol "rect "
					   (group (one-or-more nonl))
					   " " (group (one-or-more nonl))
					   "," (group (one-or-more nonl))
					   " " (group (one-or-more nonl))
					   "," (group (one-or-more nonl))
					   eol)
				       nil t))
	  (if (and
	       (> (string-to-number (match-string 4)) x (string-to-number (match-string 2)))
	       (> (string-to-number (match-string 5)) y (string-to-number (match-string 3))))
	      (setf res (match-string 1))))
	(list (cons 'href res))))))

(defun dynamic-graphs-get-rects-cmapx (file raw-x raw-y)
  "Get reference related to the screen point RAW-X RAW-Y from the cmapx FILE."
    (with-temp-buffer
      (insert-file-contents file)
      (let ((res (libxml-parse-xml-region (point-min) (point-max))))
	;; res is (map ((id . XXX)) (area ((...)))...
	(dynamic-graphs-get-rects-sexp (cddr res) raw-x raw-y))))

(defun dynamic-graphs-event-node (e)
  "Get a href value of an event E.

First a cmapx file and then imap file is tried to get the data.  The
coordinates are scaled to reflect image zooming."
  (let* ((pos (posn-x-y (event-start e)))
	 (cmapx-file (concat (file-name-sans-extension buffer-file-name) ".cmapx"))
	 (imap-file (concat (file-name-sans-extension buffer-file-name) ".imap"))
	 (scale (dynamic-graphs-get-scale (get-char-property (point-min) 'display)))
	 (x (* (car pos) scale))
	 (y (* (cdr pos) scale)))
    (cond
     ((file-readable-p cmapx-file)
      (dynamic-graphs-get-rects-cmapx cmapx-file x y))
     ((file-readable-p imap-file)
      (dynamic-graphs-get-rects-imap imap-file x y))
     (t (error "Node map not found")))))

(defun dynamic-graphs-shift-focus-or-follow-link (e)
  "Follow link or shift root node.

If the node in image has id set (cmapx only) or URL in form of
\"id:something\", there is a specific `dynamic-graphs-make-graph-fn'
and this is first click, make it new root node.

Otherwise, follow the URL with the the `dynamic-graphs-follow-link-fn'.

Argument E is the event."
  (interactive "@e")
  (let-alist (dynamic-graphs-event-node e)
    (cond
;;; This does not work well: neither .id nor .title in general match
;;; node name. Would be nice to have it work, though.

;;     ((and (= (event-click-count e) 1))
;;      (when (and .href (sit-for 0.2 t))
;;	;; wait for second click
;;	(dynamic-graphs-display-graph (file-name-base) (or .alt .title))))
     ((and .href
	   (not (eql dynamic-graphs-make-graph-fn (default-value 'dynamic-graphs-make-graph-fn)))
	   (= (event-click-count e) 1) (= 3 (cl-mismatch .href "id:")))
      (when (sit-for 0.2 t)
	;; wait for second click
	(dynamic-graphs-display-graph (file-name-base) (substring .href 3))))
     (.href (funcall dynamic-graphs-follow-link-fn .href)))))

(defun dynamic-graphs-ignore (event-or-node)
  "Work in progress, do not use.

EVENT-OR-NODE determines a node to add to the ignore list."
  (interactive "@e")
  (when (eventp event-or-node)
    (let* ((pos (posn-x-y (event-start event-or-node)))
	   (imapfile (concat (file-name-sans-extension buffer-file-name) ".cmapx"))
	   (res (dynamic-graphs-get-rects imapfile (car pos) (cdr pos))))
      (setf event-or-node
	    (when (and res (= 3 (cl-mismatch res "id:")))
	      (substring res 3)))))
  (when event-or-node
    (push event-or-node dynamic-graphs-ignore-ids)
    (dynamic-graphs-display-graph)))

;;; Key handlers
(defun dynamic-graphs-zoom-by-key (&optional keys)
  "Set distance to cut-off graph nodes based on the KEYS that invoked it."
  (interactive (list (this-command-keys)))
  (setq-local dynamic-graphs-filters
	      (mapcar (lambda (a) (if (integerp a)
				      (- (aref keys 0) 48)
				    a))
		      dynamic-graphs-filters))
  (dynamic-graphs-display-graph))

(defun dynamic-graphs-toggle-cycles ()
  "Toggle whether cycles should be broken."
  (interactive)

  (setq-local dynamic-graphs-filters
	      (if (member 'remove-cycles dynamic-graphs-filters)
		  (remove 'remove-cycles dynamic-graphs-filters)
		(append dynamic-graphs-filters '(remove-cycles))))
  (dynamic-graphs-display-graph))

(defun dynamic-graphs-clean-root ()
  "Clean root to display full graph."
  (interactive)
  (setq dynamic-graphs-root nil)
  (dynamic-graphs-display-graph)  )

(defun dynamic-graphs-set-engine (&optional engine)
  "Locally set ENGINE for graph creation."
  (interactive (cdr (read-multiple-choice "Engine: "
					  (mapcar (lambda (a) `(,(car a) ,(cadr a))) dynamic-graphs-engines))))
  (setq-local dynamic-graphs-cmd engine)
  (dynamic-graphs-display-graph))

;;; Minor mode with handlers
(defvar dynamic-graphs-keymap
  (let ((km (make-sparse-keymap)))
    (define-key km [mouse-1] 'dynamic-graphs-shift-focus-or-follow-link)
    (define-key km "1" 'dynamic-graphs-zoom-by-key)
    (define-key km "2" 'dynamic-graphs-zoom-by-key)
    (define-key km "3" 'dynamic-graphs-zoom-by-key)
    (define-key km "4" 'dynamic-graphs-zoom-by-key)
    (define-key km "5" 'dynamic-graphs-zoom-by-key)
    (define-key km "6" 'dynamic-graphs-zoom-by-key)
    (define-key km "7" 'dynamic-graphs-zoom-by-key)
    (define-key km "8" 'dynamic-graphs-zoom-by-key)
    (define-key km "9" 'dynamic-graphs-zoom-by-key)
    (define-key km "c" 'dynamic-graphs-toggle-cycles)
    (define-key km "e" 'dynamic-graphs-set-engine)
    (define-key km "!" 'dynamic-graphs-save-gv)
    (define-key km "p" 'dynamic-graphs-save-pdf)
    (define-key km "/" 'dynamic-graphs-clean-root)

    km))

(define-minor-mode dynamic-graphs-graph-mode "Local mode for dynamic images.

Allows shift focus to a different mode, and zoom in or zoom out
to see less or more distant nodes.

\\{dynamic-graphs-keymap}" nil "(dyn)" dynamic-graphs-keymap
(setq-local revert-buffer-function (lambda (_a _b)
				     (dynamic-graphs-display-graph))))

(provide 'dynamic-graphs)
;;; dynamic-graphs.el ends here

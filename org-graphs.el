;;; org-graphs.el --- Manipulation with graphviz graphs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020  Tomas Zellerin
;;
;; Author: Tomas Zellerin <tomas@zellerin.cz>
;; Keywords: tools
;; Package-version: 0.9
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
;;
;;; Commentary:
;;
;; Make dynamic graphs: take a graph, apply some filters, and display
;; it as an image. The graph can be either a function that inserts the
;; graph (and is called for each redisplay), or a buffer that can
;; change. An imap file is created in addition to the image so that
;; clicks on image can be related to individual nodes (TODO: only for
;; rectangles so far)
;;
;; The filters can apply both enhancing operations (add colors, ...)
;; and more complicated operations coded in gvpr. As a special case
;; there is a filter that removes all nodes that are more distant than
;; a parameter from a root node.
;;
;; The image is displayed with a specialized minor mode.
;; Predefined key bindings on the displayed image in this mode include:
;; - e (org-graphs-set-engine) change grahviz engine (dot, circo, ...)
;; - c (org-graphs-remove-cycles) change whether cycles are removed
;; - 1-9 (org-graphs-zoom-by-key) set maximum displayed distance from a root node
;; - mouse-1 (org-graphs-handle-click) follow link defined in imap
;;   file - that is, in URL attribute of the node. Link is followed by
;;   org-link-open-from-string, which is the only actual link to org
;;   mode
;; - S-mouse-1 (org-graphs-shift-focus) if the link for node is
;;   id:<node name>, extract node name and make it a new
;;   root. Predefined filter `node-refs' set hrefs in such way. This
;;   simplifies things when nodes relate to org mode items, but
;;   actually does not make much sense otherwise.
;;
;; Example:
;;
;; (org-graphs-display-graph "test" nil
;;		   (lambda ()
;;		     (insert "digraph Gr {A->B B->C C->A A->E->F->G}"))
;;		   '(2 remove-cycles "N {style=\"filled\",fillcolor=\"lightgreen\"}"
;;                   node-refs boxize))
;;
;;; Code:

(defcustom org-graphs-ignore-ids ()
  "IDs that are ignored in graphs.

The ids are taken as string and any lines in the graph that
contains any of them is removed, so it better is not too vague."
  :group 'org-graph
  :type '(repeat string))

(defcustom org-graphs-filters '(3 default remove-cycles)
  "Default filter for org-graph.

This should be list of filters.

 Each filter can be:
- a string that denotes either name of a gvpr file to be applied or
  direct gvpr transformation,
- an integer that denotes that only nodes with distance from root less
  or equal to the number should be kept,
- symbol `remove-cycles' that causes cycles removal,
- symbol `debug' that does not filter the graph, but displays in the
  `*messages*' buffer current graph."
  :group 'org-graph
  :type '(repeat
	  (choice (integer :tag "Maximum distance from root to keep")
		  (string :tag "gvpr code to apply")
		  (file :must-match t :tag "gvpr source file to apply")
		  (const :tag "Remove cycles in graph" remove-cycles)
		  (symbol :tag "Reference to org-graphs-transformations")
		  (debug :tag "Dump transformed graph to *messages*"))))

(put 'org-graphs-filters 'permanent-local t)

(defcustom org-graphs-cmd "twopi"
  "Command to create final image."
  :group 'org-graphs
  :type '(choice (const "twopi")
		 (const "dot")
		 (const "neato")
		 (const "circo")))

(put 'org-graphs-cmd 'permanent-local t)

(defcustom org-graphs-image-directory (file-truename temporary-file-directory)
  "Command to create final image."
  :group 'org-graphs
  :type 'directory)

(defvar org-graphs-make-graph-fn nil
  "Function that creates a graph.")

(make-variable-buffer-local 'org-graphs-make-graph-fn)
(put 'org-graphs-filters 'permanent-local t)

(defvar-local org-graphs-root nil
  "Root node if set")

(defcustom org-graphs-transformations
  '((boxize . "N {shape=\"box\"}")
    (default . "BEG_G{overlap=\"false\", fontname=\"Courier\"}
N[dist<0.5]{style=\"filled\",fillcolor=\"yellow\",fontsize=\"22\"}
N[dist>1.5]{penwidth=\"0.1\",fontsize=\"12\"}
N[dist>2.5]{shape=\"none\",fontsize=\"8\"}"))
  "Named predefined transformations gvdr snippets. The names can be
  used in the org-graphs-filters to make it more manageable."
  :group 'org-graphs
  :type '(alist :key-type symbol :value-type string))

(defun org-graphs-unscale ()
  "Scale the image to 100% zoom."
  (interactive)
  (let* ((image (image--get-imagemagick-and-warn))
         (new-image (image--image-without-parameters image)))
    (setcdr image (cdr new-image))
    (plist-put (cdr image) :scale 1.0)))

(defun org-graphs-get-scale ()
  "Get scale of the image."
  (let* ((image (image--get-imagemagick-and-warn))
         (new-image (image--image-without-parameters image)))
    (image--current-scaling image new-image)))

(defun org-graphs-shift-focus (e)
  "Make the node under cursor new root."
  (interactive "@e")
  (let* ((pos (posn-x-y (event-start e)))
	 (imapfile (concat (file-name-sans-extension buffer-file-name) ".imap"))
	 (res (org-graphs-get-rects imapfile (car pos) (cdr pos))))
    (when (and res (= 3 (cl-mismatch res "id:")))
      (org-graphs-display-graph (file-name-base) (substring res 3)))))

(defun org-graphs-rebuild-graph (base-file-name root make-graph-fn &optional filters)
  "Create png and imap files in the `org-graphs-image-directory'
directory named by the `base-file-name'.

The `MAKE-GRAPH-FN' inserts the original graph into the buffer. The
graph is modified as specified by the list `FILTERS'. See `org-graphs-filters' for the syntax.

Finally, process the graph with `org-graphs-cmd' to create image and imap file from the final graph.

Return the graph as the string (mainly for debugging purposes).
"
  (let ((cmd org-graphs-cmd))
    (cl-flet ((cmd (name &rest pars)
		   (message "%s %S" name pars)
		   (let ((before (buffer-string))
			 (res (apply 'call-process-region (point-min) (point-max) name pars)))
		     (unless (or (zerop res) (and (equal name "acyclic") (< res 255)))
		       (error (format "failed %s: %s->%s" name before (buffer-string)))))))
      (with-temp-buffer
	(funcall make-graph-fn)
	(dolist (filter (or filters org-graphs-filters))
	  (when (symbolp filter)
	    (setq filter
		  (or (cdr (assoc filter org-graphs-transformations))
		      filter)))
	  (cond
	   ((and (stringp filter) (file-exists-p filter))
	    (cmd "gvpr" t t nil "-c" "-qf" filter))
	   ((and (stringp filter))
	    (cmd "gvpr" t t nil "-c" "-q" filter))
	   ((integerp filter)
	    (when root
	      (cmd "dijkstra" t t nil root)
	      (cmd "gvpr" t t nil "-c" "-a" (format "%d.0" filter)
		   "-q" "BEGIN{float maxdist; sscanf(ARGV[0], \"%f\", &maxdist)}
 N[!dist || dist >= maxdist] {delete(root, $)}")))
	   ((eq filter 'remove-cycles)
	    (cmd "acyclic" t t)
	    (cmd "tred" t t))
	   ((eq filter 'debug)
	    (message "Filters debug: %s" (buffer-string)))
	   ((and (consp filter)
		 (eq (car filter) 'ignore))
	    (delete-matching-lines (regexp-opt (cdr filter))
				   (point-min) (point-max)))
	   (t (error "Unknown transformation %s" filter))))
	(message "cmd: %s" org-graphs-cmd)
	(dolist (type '("png" "imap"))
	  (cmd cmd
	       nil nil nil "-o" (concat org-graphs-image-directory "/" base-file-name "." type) "-T" type))
	(buffer-string)))))

(defun org-graphs-rebuild-and-display (base-file-name root make-graph-fn filters)
  "Redisplay the graph in the current buffer.

  Specifically set local values of some global parameters and run
  `org-graphs-rebuild-graph' with appropriate arguments.

  Display the resulting png file or, when there is already a buffer
  with the file, redisplay."
  (org-graphs-rebuild-graph base-file-name root make-graph-fn filters)
  ;; display it
  (let ((old-image-buffer (get-file-buffer (concat org-graphs-image-directory base-file-name ".png"))))
    (if (null old-image-buffer)
	(find-file (concat org-graphs-image-directory base-file-name ".png"))
      (switch-to-buffer old-image-buffer)
      (setq-local revert-buffer-function 'revert-buffer--default)
      (revert-buffer t t)))
  (setq-local org-graphs-filters filters)
  (setq-local org-graphs-root root)
  (setq-local org-graphs-make-graph-fn make-graph-fn)
  (org-graphs-graph-mode t))

(defun org-graphs-display-graph (&optional base-file-name root make-graph-fn filters)
  (org-graphs-rebuild-and-display base-file-name
			(or root org-graphs-root)
			(or make-graph-fn org-graphs-make-graph-fn)
			(or filters org-graphs-filters)))

(defun org-graphs-display-graph-buffer (root filters)
  (interactive (list nil org-graphs-filters))
  (let ((buffer (current-buffer)))
    (org-graphs-rebuild-and-display (file-name-base)
			  root
			  (lambda () (insert-buffer-substring buffer))
			  filters)))


;;; Callbacks to be bound on keys or events

(defun org-graphs-handle-click (e)
  "Follow URL link in an image."
  (interactive "@e")
  (let* ((pos (posn-x-y (event-start e)))
	 (imapfile (concat (file-name-sans-extension buffer-file-name) ".imap"))
	 (res (org-graphs-get-rects imapfile (car pos) (cdr pos))))
    (when res
      (message (format res))
      (org-link-open-from-string res))))

(defun org-graphs-get-rects (file x y)
  (when (file-readable-p file)
    (let ((scale (org-graphs-get-scale)))
      (setq x (/ x scale)
	    y (/ y scale)))
    (save-mark-and-excursion
      (find-file file)
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
	(kill-buffer)
	res))))


(defun org-graphs-zoom-by-key ()
  (interactive)
  (org-graphs-display-graph
   (file-name-base)
   org-graphs-root
   org-graphs-make-graph-fn
   (mapcar (lambda (a) (if (integerp a)
			   (- (aref (this-command-keys) 0) 48)
			 a))
	   org-graphs-filters)))

(defvar org-graphs-keymap (make-sparse-keymap))

(defun org-graphs-ignore (event-or-node)
  (interactive "@e")
  (when (eventp event-or-node)
    (let* ((pos (posn-x-y (event-start event-or-node)))
	   (imapfile (concat (file-name-sans-extension buffer-file-name) ".imap"))
	   (res (org-graphs-get-rects imapfile (car pos) (cdr pos))))
      (setf event-or-node
	    (when (and res (= 3 (cl-mismatch res "id:")))
	      (substring res 3)))))
  (when event-or-node
    (push event-or-node org-graphs-ignore-ids)
    (org-graphs-display-graph)))

(defun org-graphs-toggle-cycles ()
  (interactive)
  (if (member 'remove-cycles org-graphs-filters)
      (setq-local org-graphs-filters (remove 'remove-cycles org-graphs-filters))
    (setq-local org-graphs-filters (append org-graphs-filters '(remove-cycles))))
  (org-graphs-display-graph))

(defun org-graphs-set-engine (&optional engine)
  (interactive (cdr (read-multiple-choice "Engine: "
					  '((?d "dot")
					    (?c "circo")
					    (?n "neato")
					    (?t "twopi")))))
  (setq-local org-graphs-cmd engine)
  (org-graphs-display-graph))

(define-key org-graphs-keymap [mouse-1] 'org-graphs-handle-click)
(define-key org-graphs-keymap [S-mouse-1] 'org-graphs-shift-focus)
(define-key org-graphs-keymap [S-down-mouse-1] 'org-graphs-shift-focus )
(define-key org-graphs-keymap "1" 'org-graphs-zoom-by-key)
(define-key org-graphs-keymap "2" 'org-graphs-zoom-by-key)
(define-key org-graphs-keymap "3" 'org-graphs-zoom-by-key)
(define-key org-graphs-keymap "4" 'org-graphs-zoom-by-key)
(define-key org-graphs-keymap "5" 'org-graphs-zoom-by-key)
(define-key org-graphs-keymap "6" 'org-graphs-zoom-by-key)
(define-key org-graphs-keymap "7" 'org-graphs-zoom-by-key)
(define-key org-graphs-keymap "8" 'org-graphs-zoom-by-key)
(define-key org-graphs-keymap "9" 'org-graphs-zoom-by-key)
(define-key org-graphs-keymap "c" 'org-graphs-toggle-cycles)
(define-key org-graphs-keymap "e" 'org-graphs-set-engine)
(define-key org-graphs-keymap (kbd "<S-mouse-3>") 'org-graphs-ignore)
(define-key org-graphs-keymap (kbd "<S-down-mouse-3>") 'org-graphs-ignore)

(define-minor-mode org-graphs-graph-mode "Local mode for ZK images.

Allows shift focus to a different mode, and zoom in or zoom out
to see less or more distant nodes.

\\{org-graphs-keymap}" nil "(ZK)" org-graphs-keymap
  (setq-local revert-buffer-function (lambda (_a _b)
				       (org-graphs-display-graph))))

(provide 'org-graphs)
;;; org-graphs.el ends here

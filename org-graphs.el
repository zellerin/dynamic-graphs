;;; org-graphs.el --- Manipulation with graphviz graphs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020  Tomas Zellerin
;;
;; Author: Tomas Zellerin <tomas@zellerin.cz>
;; Keywords: tools
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
;; Make dynamic graphs.
;;
;; Example:
;;
;; (org-graphs-display-graph 3 "A"
;;		   (lambda ()
;;		     (insert "digraph G {A->B B->C C->A A->F F->G G->H }")))
;;
;;; Code:
;;
;; removed: directories items

(defvar org-graphs-debug nil)

(defcustom org-graphs-ignore-ids ()
  "IDs that are ignored in graphs.

The ids are taken as string and any lines in the graph that
contains any of them is removed, so it better is not too vague."
  :group 'org-graph
  :type '(repeat string))

(defcustom org-graphs-filters ()
  "Default filter for org-graph.

This should be a gvpr file."
  :group 'org-graph
  :type '(file :must-match t))

(defcustom org-graphs-remove-cycles ()
  "Remove cycles from graphs.

When true, runs acyclic on the graph to open any cycles."
  :group 'org-graph
  :type 'boolean)

(defcustom org-graphs-cmd "twopi"
  "Command to create final image."
  :group 'org-graphs
  :type '(choice (const "twopi")
		 (const "dot")
		 (const "neato")
		 (const "circo")))

(defcustom org-graphs-image-directory (file-truename temporary-file-directory)
  "Command to create final image."
  :group 'org-graphs
  :type 'directory)

(defvar org-graphs-make-graph-fn nil
  "Function that creates a graph.")

(make-variable-buffer-local 'org-graphs-make-graph-fn)

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
      (kill-buffer)
      (org-graphs-display-graph nil (substring res 3)))))

(defun org-graphs-rebuild-graph (root radius make-graph-fn)
  "Create png and imap files in the `org-graphs-image-directory' directory named by the `root'.

The `make-graph-fn' inserts the original graph into the buffer. The
graph is
- optionally filtered for ignored strings,
- the nodes are tagged with the distance from the root node
- the gvpr commands in `org-graphs-filter' are applied,
- and optionally cycles are removed to simplify the structure

The gvpr command is passed two arguments, root node (so that it can be
set specifically), and value of `radius'. The idea
is that it is the maximum distance from root node that is kept."
  (let ((remove-cycles org-graphs-remove-cycles)
	(ignore org-graphs-ignore-ids))
    (cl-flet ((cmd (name &rest pars)
	       (let ((before (buffer-string))
		     (res (apply 'call-process-region (point-min) (point-max) name pars)))
		 (unless (or (zerop res) (and (equal name "acyclic") (< res 255)))
		   (error (format "failed %s: %s->%s" name before (buffer-string)))))))
      (with-temp-buffer
	(funcall make-graph-fn)
	(when ignore
	  (delete-matching-lines (regexp-opt ignore) (point-min) (point-max)))
	(cmd "dijkstra" t t nil root)
	(when org-graphs-filter
	  (cmd "gvpr" t t nil "-c" "-a"
	       (format "%s %d.0" root
		       (if (numberp radius) radius 3))
	       "-qf" org-graphs-filter))
	(when remove-cycles
	  (cmd "acyclic" t t)
	  (cmd "tred" t t))
	(dolist (type '("png" "imap"))
	  (cmd
	   org-graphs-cmd
	   nil nil nil "-o" (concat org-graphs-image-directory "/" root "." type) "-T" type))))))

 ;; (defun org-graphs-display (image-name)
 ;;     (let ((buf (find-buffer-visiting image-name)))
 ;;       (if (null buf)
 ;; 	   (find-file image-name)
 ;; 	 (switch-to-buffer buf)
 ;; 	 (revert-buffer t t)))
 ;;     (org-graphs-rebuild-graph t))

(defun org-graphs-rebuild-and-display (radius root make-graph-fn ignore remove-cycles)
  "Redisplay the graph in the current buffer.

  Specifically set local values of some global parameters and run
  `org-graphs-rebuild-graph' with appropriate arguments.

  Display the resulting png file or, when there is already a buffer
  with the file, redisplay."
  (org-graphs-rebuild-graph root radius make-graph-fn)
  ;; display it
  (let ((old-image-buffer (get-file-buffer (concat org-graphs-image-directory root ".png"))))
    (if (null old-image-buffer)
	(find-file (concat org-graphs-image-directory  root ".png"))
      (switch-to-buffer old-image-buffer)
      (setq-local revert-buffer-function 'revert-buffer--default)
      (revert-buffer t t)))
   (setq-local org-graphs-ignore-ids ignore)
   (setq-local org-graphs-remove-cycles remove-cycles)
   (setq-local org-graphs-make-graph-fn make-graph-fn)
  (org-graphs-graph-mode t))

(defun org-graphs-display-graph (radius root &optional make-graph-fn)
  (org-graphs-rebuild-and-display radius root (or make-graph-fn org-graphs-make-graph-fn)
			org-graphs-ignore-ids org-graphs-remove-cycles))


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
    (let ((scale (zettelkasten-graph-get-scale)))
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
  (org-graphs-display-graph (- (aref (this-command-keys) 0) 48) (file-name-base)))

(defvar org-graphs-graph-keymap (make-sparse-keymap))

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
    (org-graphs-display-graph nil (file-name-base))))

(defun org-graphs-toggle-cycles ()
  (interactive)
  (setq-local org-graphs-remove-cycles (not org-graphs-remove-cycles))
  (org-graphs-display-graph nil (file-name-base)))

(bind-key "<mouse-1>" 'org-graphs-handle-click org-graphs-keymap)
(bind-key "<S-mouse-1>" 'org-graphs-shift-focus org-graphs-graph-keymap)
(bind-key "<S-down-mouse-1>" 'org-graphs-shift-focus  org-graphs-graph-keymap)
(bind-key "0" 'org-graphs-graph-unscaled org-graphs-graph-keymap)
(bind-key "1" 'org-graphs-zoom-by-key org-graphs-graph-keymap)
(bind-key "2" 'org-graphs-zoom-by-key org-graphs-graph-keymap)
(bind-key "3" 'org-graphs-zoom-by-key org-graphs-graph-keymap)
(bind-key "4" 'org-graphs-zoom-by-key org-graphs-graph-keymap)
(bind-key "5" 'org-graphs-zoom-by-key org-graphs-graph-keymap)
(bind-key "6" 'org-graphs-zoom-by-key org-graphs-graph-keymap)
(bind-key "7" 'org-graphs-zoom-by-key org-graphs-graph-keymap)
(bind-key "8" 'org-graphs-zoom-by-key org-graphs-graph-keymap)
(bind-key "9" 'org-graphs-zoom-by-key org-graphs-graph-keymap)
(bind-key "<S-mouse-3>" 'org-graphs-ignore org-graphs-graph-keymap)
(bind-key "<S-down-mouse-3>" 'org-graphs-ignore org-graphs-graph-keymap)

(define-minor-mode org-graphs-graph-mode "Local mode for ZK images.

Allows shift focus to a different mode, and zoom in or zoom out
to see less or more distant nodes.

\\{org-graphs-graph-keymap}" nil "(ZK)" org-graphs-graph-keymap
  (setq-local revert-buffer-function (lambda (_a _b)
				       (org-graphs-display-graph '(4) (file-name-base)))))

(provide 'org-graphs)
;;; org-graphs.el ends here

(defun dynamic-graphs-test-image (expects &optional expects-not)
  ;; is an image?
  (should (get-char-property (point-min) 'display))
  ;; is a png image?
  (should (equal "PNG" (buffer-substring-no-properties 2 5)))
  (with-temp-buffer
    (insert-file-literally "/tmp/ert-test.imap")
    (dolist (exp expects)
      (goto-char (point-min))
      (re-search-forward exp))
    (dolist (exp expects-not)
      (goto-char (point-min))
      (should-error (re-search-forward exp)))))

(ert-deftest from-fn ()
  (save-window-excursion
    (with-temp-buffer
      (setq-local dynamic-graphs-cmd "neato")
      (dynamic-graphs-display-graph
		     "ert-test" "A"
		     (lambda ()
		       (insert "digraph Gr {A -> B -> C}"))
		     '(2 node-refs boxize default))
      (dynamic-graphs-test-image '("rect id:A 5,5"))
      (dolist (eng (mapcar #'cadr dynamic-graphs-engines))
	(dynamic-graphs-set-engine eng)
	(dynamic-graphs-test-image '("rect id:A" "rect id:B")))
      (dynamic-graphs-set-engine "neato")
      ;; test that key 1 zooms out properly
      (dynamic-graphs-zoom-by-key [49])
      (dynamic-graphs-test-image '("rect id:A") '("rect id:B"))
      ;; zoom out
      (dynamic-graphs-zoom-by-key [55])
      ;; verify C position
      (dynamic-graphs-test-image '("rect id:A" "rect id:B" "rect id:C 75,134 147,182"))
      ;; focus on C
      (dynamic-graphs-shift-focus-or-follow-link
       '(mouse-1 (nil 1 (122 . 160) 26976289 nil 1 (0 . 0)
		      (image :type imagemagick :file "/tmp/ert-test.png" :scale 1 :max-width 1880 :max-height 492)
		      (122 . 160)
		      (152 . 187))))
      (dynamic-graphs-zoom-by-key [49])
      (dynamic-graphs-test-image '("rect id:C") '("rect id:A" "rect id:B")))))

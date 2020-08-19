(ert-deftest from-fn ()
  (save-window-excursion
    (let ((dynamic-graphs-cmd "neato"))
      (dynamic-graphs-display-graph "ert-test" nil
				    (lambda ()
				      (insert "digraph Gr {A [URL=\"http://example.com\", pos=\"70,70!\"] A -> B -> C -> A ->E->F->G}"))
				    '(2 "N {style=\"filled, rounded\",fillcolor=\"green\"}"
					node-refs boxize default)))
    (should (get-char-property (point-min) 'display))
    ;; is png image?
    (should (equal "PNG" (buffer-substring-no-properties 2 5)))
    ))

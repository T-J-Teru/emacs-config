;; Allow jumping between matching parenthesis.

(defun jump-to-matching-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert a hash."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(provide 'mparen)


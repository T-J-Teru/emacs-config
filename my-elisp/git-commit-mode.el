;; Mode for highlighting git commit messages.

(defconst git-commit-keywords
  (list
   '("^[ \t]*\\(#.*\\)$"          . font-lock-comment-face))
  "Highlight commit comments")

(defun git-commit-mode ()
  "Enter git commit mode"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'git-commit-mode)
  (flyspell-mode)
  (auto-fill-mode)
  (set (make-local-variable 'font-lock-defaults) '(git-commit-keywords))
  (font-lock-mode)
  (setq mode-name "GIT"))

(provide 'git-commit-mode)
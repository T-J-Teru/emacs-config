;; Function to toggle menu and tool bars on and off.
(defun toggle-menubar-and-toolbar ()
  "Switches off both menu and tool bar."
  (interactive)
  (if window-system
      ;; In X mode we have menu-bar and tool-bar, rotate through all
      ;; the combinations of having each on or off. Using "M" for
      ;; menu-bar on, and "m" for menu-bar off, and "T" for tool-bar
      ;; on, and "t" for tool-bar off, then the sequence is:
      ;;      (m,t)->(M,t)->(M,T)->(m,T)->(m,t)
      (if tool-bar-mode 
          (if menu-bar-mode
              (menu-bar-mode -1)
            (tool-bar-mode -1))
        (if menu-bar-mode 
            (tool-bar-mode 1)
          (menu-bar-mode 1)))
    ;; In terminal mode we only have the menu-bar, just toggle it on
    ;; and off.
    (if menu-bar-mode
        (menu-bar-mode -1)
      (menu-bar-mode 1))))

;; Initially, we want the menu and tool bars to be off.  On earlier
;; versions of emacs this is done by setting attributes in the
;; frame-alist. (See my general init.el code).
(if (>= emacs-major-version 24)
    (progn
      (menu-bar-mode -1)
      (tool-bar-mode -1)))

(provide 'menu-toggle)

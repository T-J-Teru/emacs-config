;; ========================================================================
;; Create a new style for editing C/C++ code, and select this style.
(defun andrew-cc-mode/set-style ()
  (c-add-style
   "ANDREW-C"
   '(
     "gnu"
     (c-basic-offset . 2)
     (c-hanging-braces-alist . (
				(substatement-open before after)
				(substatement-close before after)
				))
     (c-offsets-alist . (
                         (block-open . +)
                         (defun-open . 0)
                         (brace-list-open . 0)
                         ))
     ) t)
  )

;; ========================================================================
;; Face used for highlighting printf format specifiers.
(defvar font-lock-format-specifier-face
  'font-lock-format-specifier-face
  "Face name to use for format specifiers.")

(defface font-lock-format-specifier-face
  '((t (:foreground "cyan1")))
  "Font Lock mode face used to highlight format specifiers."
  :group 'font-lock-faces)

(defun andrew-cc-mode/printf-specifiers ()
  (font-lock-add-keywords 
   nil
   '(("[^%]\\(%\\([[:digit:]]+\\$\\)?[-+' #0*]*\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\(\\.\\([[:digit:]]*\\|\\*\\|\\*[[:digit:]]+\\$\\)\\)?\\([hlLjzt]\\|ll\\|hh\\)?\\([aAbdiuoxXDOUfFeEgGcCsSpn]\\|\\[\\^?.[^]]*\\]\\)\\)"
      1 font-lock-format-specifier-face t)
     ("\\(%%\\)" 
      1 font-lock-format-specifier-face t)) ))

;; ========================================================================
;; Settings for editing C/C++ files.
(defun andrew-cc-mode ()
  ;; Pick a nice code layout style.
  (andrew-cc-mode/set-style)
  
  ;; Auto newline, and hungry delete.
  (c-toggle-auto-hungry-state t)

  ;; Printf format specifier highlighting.
  (andrew-cc-mode/printf-specifiers)

  ;; Allow things to be hidden.
  (hs-minor-mode)

  ;; Wrap long comments.
  (auto-fill-mode)

  ;; Spell checking in comments / strings.
  (flyspell-prog-mode)

  ;; Jumping to functions by name.
  (local-set-key (kbd "C-x g") 'imenu)

  ;; Make more use of 80 character display.
  (setq fill-column 75)

  ;(require 'auto-complete)
  ;(global-auto-complete-mode t)
  ;(setq ac-auto-start 3)
  ;(define-key ac-complete-mode-map "\M-/" 'ac-stop)  
  )

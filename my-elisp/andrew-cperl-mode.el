;;============================================================================
;; Configuration for cperl mode.
(defun andrew-cperl-mode/variable-setup ()
  (setq cperl-auto-newline t)
  (setq cperl-autoindent-on-semi t)
  (setq show-trailing-whitespace 'nil)
  (setq cperl-extra-newline-before-brace t)
  (setq cperl-extra-newline-before-brace-multiline t)
  (setq cperl-font-lock t)
  (setq cperl-hairy 'nil)
  ;;
  ;; Prevent underline being used for end of line whitespace. This used to
  ;; work upto emacs 23 but then got broken.
  (if (<= emacs-major-version 23)
      (progn
        (setq cperl-invalid-face nil)
        (setq cperl-highlight-variables-indiscriminately 't)))
  )

;;============================================================================
;; This will configure some faces for me
;;
;; Faces for cperl-mode.
(defun andrew-cperl-mode/face-changes ()
  (set-face-attribute 'cperl-array-face nil
                      :foreground "light blue"
                      :weight 'light
                      :slant 'italic
                      :background 'unspecified)
  
  (set-face-attribute 'cperl-hash-face nil
                      :foreground "Red"
                      :slant 'italic
                      :weight 'bold
                      :background 'unspecified)) 


;;============================================================================
;; Gives perldoc at word under point when in cperl-mode by pressing F1
(defun andrew-cperl-mode/provide-perldoc ()
  "A hook which binds F1 to `cperl-perldoc-at-point'."
  (local-set-key [f1] '(lambda ()
                         (interactive)
                         (cperl-perldoc-at-point))))

;;============================================================================
;; Create a new perl function, and insert a prototype. If we are inserting
;; into a perl module then use an access modifier, otherwise don't bother.
(defun insert-perl-method (name) 
  (interactive)
  (progn
    ;; Work out if we are in a perl module, or perl script
    (let* ((is-perl-module (string-match "\.pm$" (buffer-file-name))))
      ;; Start inserting the new function.
      (insert "\n=pod\n\n")
      ;; Only modules get public/private type comment.
      (if is-perl-module
          (insert (format "=item I<Private>: B<%s>\n\n" name))
        (insert (format "=item B<%s>\n\n" name)))
      ;; More function comment and code...
      (insert "Currently undocumented.\n\n")
      (insert "=cut\n\n")
      (insert (format "sub %s {\n" name))
      ;; Only modules expect to be object-oriented.
      (if is-perl-module (insert "  my $self = shift;\n"))
      (insert "  ")
      ;; Store the pointer - this is where it will be left, ready
      ;; to type the code for the function body.
      (set 'stored-point-pos (point))
      (insert "\n}\n\n")
      (insert "#========================================================================#\n")
      ;; And move the point back to the stored position.
      (goto-char stored-point-pos)
      )))

(defun start-new-perl-function (f-name)
  "Using my perl template insert a new perl function, and position the point ready to start typing"

  ;; Provide a default argument of the word the curser is currently on.
  (interactive 
   (list
    (let* ((default (current-word t))
          (basic-prompt "Function name")
          (prompt (if default
                      (format "%s: (default %s) " basic-prompt default)
                      (format "%s: " basic-prompt)))
          (name (read-string prompt)))
      (if (equal name "") default name))))
  
  ;; Now check that we can identify where to put the new method, and place it.
  (let ((current-point-position (point)))
    (progn 
      ;; Move to start of buffer
      (goto-char (point-min))
      ;; Look method with this name
      (if (re-search-forward (format "sub +%s \\((\\|{\\)" f-name) (point-max) t)
          ;;  if found then error name clash
          (error "Function already exists"))
      ;; Move to start of buffer
      (goto-char (point-min))
      ;; Look for place to insert new method
      (if (not (re-search-forward "^=head1 METHODS" (point-max) t))
          ;;  if not found then error, failed to insert new method
          (error "Failed to find start of METHOD pod"))      
      (if (not (re-search-forward "^=cut *\n.*\n#=+# *\n" (point-max) t))
          ;;  if not found then error, failed to insert new method
          (error "Failed to find end of METHOD pod"))
      ;;  insert the new method
      (insert-perl-method f-name)      
      )))

;;==========================================================================
;; Arrange for perl specific key bindings to be set up.
(defun andrew-cperl-mode/keybindings ()
  ( local-set-key (kbd "C-x /") 'start-new-perl-function) )

(defun andrew-cperl-mode/setup-style ()
  (add-to-list 'cperl-style-alist '("AndrewPerl"
                                    (cperl-indent-level . 2)
                                    (cperl-brace-offset . 0)
                                    (cperl-continued-brace-offset . -2)
                                    (cperl-label-offset . -2)
                                    (cperl-continued-statement-offset . 2)
                                    (cperl-brace-imaginary-offset . 0)
                                    (cperl-min-label-indent . 1)
                                    (cperl-close-paren-offset . -1)
                                    ))
  (cperl-set-style "AndrewPerl"))

;;============================================================================
;; Function that gets called from cperl-mode-hook to configure cperl-mode.
(defun andrew-cperl-mode ()
  (andrew-cperl-mode/variable-setup)
  (andrew-cperl-mode/provide-perldoc)
  (andrew-cperl-mode/face-changes)
  (andrew-cperl-mode/keybindings)
  (andrew-cperl-mode/setup-style)

  ;; Turn on the left margin if this feature is available.
  (if (boundp 'fci-mode)
      (progn
        (setq fill-column 75) 
        (setq fci-style 'rule)
        (fci-mode))))

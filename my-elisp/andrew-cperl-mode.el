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
(defun andrew-cperl/insert-sub (name) 
  (interactive)
  (if (or (not name) (string= name ""))
      (error "Invalid subroutine name"))
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
      (save-excursion
        (insert "\n}\n\n")
        (insert "#========================================================================#\n"))
      (point)
      )))

;; Take the name of a subroutine, return the point position for the start
;; of the subroutine line, or nil if the subroutine can't be found.
(defun andrew-cperl/find-existing-sub (sub-name)
  (save-excursion
    ;; Move to start of buffer
    (goto-char (point-min))
    ;; Look method with this name
    (if (re-search-forward (format "sub +%s \\((\\|{\\)" sub-name) (point-max) t)
        ;;  if found then error name clash
        (progn
          (beginning-of-line)
          (point))
      nil)))

;; Return the point position at which a new subroutine should be inserted.
;; Will raise an error if the position can't be determined.
(defun andrew-cperl/find-new-sub-position ()
  (save-excursion
    (goto-char (point-min))
    (if (not (re-search-forward "^=head1 METHODS" (point-max) t))
        ;;  if not found then error, failed to insert new method
        (error "Failed to find start of METHOD pod"))
    (if (not (re-search-forward "^=cut *\n.*\n#=+# *\n" (point-max) t))
        ;;  if not found then error, failed to insert new method
        (error "Failed to find end of METHOD pod"))
    (point)))

(defun andrew-cperl/new-sub (f-name)
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
  (let ((destination-pos (point)))
    (save-excursion
      (let ((existing-sub-pos (andrew-cperl/find-existing-sub f-name)))
        (if (not existing-sub-pos)
            (progn
              (goto-char (andrew-cperl/find-new-sub-position))
              (set 'destination-pos (andrew-cperl/insert-sub f-name)))
          (set 'destination-pos existing-sub-pos))))
              
    ;; If we get here with no errors then the function was created, before
    ;; we move there, push the mark so we can always come back here
    ;; quickly.
    (push-mark (point) t)
    (goto-char destination-pos)))

;==========================================================================
(defun andrew-cperl/current-sub-name ()
  "Return the name of the perl subroutine we're currently \"within\" in the 
current buffer.  Within is interpreted rather loosely, as this function tries 
to return something sensible if the point is on the sub line, or if the point 
is within the =pod ... =cut documentation for this subroutine."
  (error "TODO: This needs fixing..."
  (save-excursion
    (save-match-data
      (let ((sub-name nil))
        (flet (sub-line? (lambda () (looking-at 
                                  "^sub \\([a-z][a-z0-9_]*\\) {")))
          (beginning-of-line)
          (if (not (sub-line?))
              (progn
                (beginning-of-defun)
                (if (not (sub-line?))
                    (error "Couldn't find subroutine start"))))
          (match-string 1)))))))

(defun andrew-cperl/toggle-visibility ()
  "Toggle the visibility comment within the =pod ... =cut section of the
documentation for the current subroutine.  This will only have an effect
within perl modules where there is a visibility specifier, in scripts this
should have no effect."
  (interactive)
  (save-excursion
    (save-match-data
      (let* ((orig-point (point))
             (sub-start (progn 
                          (if (not (beginning-of-defun))
                              (error "Can't find subroutine start"))
                          (point)))
             (sub-name (progn
                         (if (not (looking-at "^sub \\([a-z][a-z0-9_]*\\) {"))
                             (error "Couldn't match subroutine definition line"))
                         (match-string 1)))
             (sub-pod-regexp (concat "^=item.*B<" sub-name ">"))
             (sub-end (progn
                        (end-of-defun)
                        (point))))
        ;; Check that the point is inside the function.  This will fail to
        ;; do the right thing if we were originally on the sub header line,
        ;; as in this case the beginning-of-defun call will take us to the
        ;; previous subroutine.
        (if (or (< orig-point sub-start) (> orig-point sub-end))
            (error "Outside of subroutine"))
        ;; Now try to find the =pod ... =cut block for this sub.
        (goto-char sub-start)
        (skip-chars-backward "[\n\r[:space:]]")
        (beginning-of-line)
        (if (not (looking-at-p "=cut"))
            (error "Didn't find end of =pod ... =cut for subroutine"))
        (if (not (search-backward-regexp "^=pod"))
            (error "Didn't find start of =pod ... =cut for subroutine"))
        (if (not (search-forward-regexp sub-pod-regexp))
            (error "Didn't find subroutine =item line in pod"))
        (beginning-of-line)
        ;; Should now be at the start of the =item line for this
        ;; subroutine.  We now check for I<Public> or I<Private> on this
        ;; line, and if one of them is found change it to the other.
        (if (not (looking-at "^=item I<\\(\\(Public\\)\\|\\(Private\\)\\)>"))
            (error "No Public|Private found on =item line in pod"))
        (forward-char (length "=item I<"))
        ;; The match-string is extracted from the buffer, so we don't
        ;; delete until we know what it is we're going to insert.
        (if (string= (match-string 1) "Public")
            (progn
              (delete-char (length "Public"))
              (insert "Private"))
          (progn
            (delete-char (length "Private"))
            (insert "Public")))
        (beginning-of-line)
        ;; If we loaded the pulse library then pulse the line we just changed.
        (if (fboundp 'pulse-momentary-highlight-one-line)
            (pulse-momentary-highlight-one-line (point)))
        )
      )
    )
  )
  
;;==========================================================================
;; Arrange for perl specific key bindings to be set up.
(defun andrew-cperl-mode/keybindings ()
  (local-set-key (kbd "C-x /") 'andrew-cperl/new-sub)
  (local-set-key (kbd "C-x ?") 'andrew-cperl/toggle-visibility))

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
  ;; Load the cool pulse library from CEDET, if it's available.
  (require 'pulse nil t)

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

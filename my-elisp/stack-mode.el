(defvar stack-left-whitespace 8)
(defvar stack-cell-bytes 8)

(defun stack-insert-leading-whitespace ()
  (insert-char ?  stack-left-whitespace))

(defun stack-cell-char-width ()
  "Return the number characters that is the width of a stack cell."
  (* 2 stack-cell-bytes))

(defun stack-insert-line (c &optional no-border?)
  "Inserts a single line of a stack cell.  Use character C as the
content of the line.  A line is formed by first inserting
whitespace using `stack-insert-leading-whitespace', then printing
a | character, then printing 2 * `stack-cell-bytes' instances of the
character C, followed by another | character, and finally a newline.

If the optional NO-BORDER? is true then the border character is replaced
by a single whitespace."
  (let ((border-char (if no-border? ?  ?|)))
    (stack-insert-leading-whitespace)
    (insert-char border-char)
    (insert-char c (stack-cell-char-width))
    (insert-char border-char)
    (insert-char ?\n)))

(defun stack-find-stack (find-top?)
  "Move point to the beginning of the line that is either the top
ot that stack when FIND-TOP? is true, or the beginning of the line
that is the bottom of the stack when FIND-TOP? is false."
  (let ((search-pattern (make-string 
                         (stack-cell-char-width) 
                         (if find-top? ?^ ?v))))
    (if find-top? (beginning-of-buffer) (end-of-buffer))
    (if (not (if find-top?
                 (search-forward search-pattern nil t)
               (search-backward search-pattern nil t)))
        (error "Failed to find '%s'" where))
    (beginning-of-line)))

(defun stack-add-cells (&optional count at-top)
  "Insert COUNT stack cells (words) either at the top of the stack 
when AT-TOP is true, or at the bottom of the stacj when AT-TOP is false."
  (interactive)
  (if (eq count nil)
      (setq count 1))
  ;; Find the correct line depending if we're look for stack-top or
  ;; stack-bottom, move to that line.
  (stack-find-stack at-top)
  (letrec ((do-add (lambda (count at-top)
                  (if at-top
                      (progn
                        (next-line)
                        (stack-insert-line ?-)
                        (stack-insert-line ? ))
                    (progn
                      (stack-insert-line ? )
                      (stack-insert-line ?-)))
                  (if (> count 1)
                      (funcall do-add (- count 1) at-top)))))
    (funcall do-add count at-top))
  )

(defun stack-grow-up (&optional count)
  "Add COUNT cells to the top of the stack, COUNT will defaul to 1."
  (interactive)
  (if (eq count nil)
      (setq count 1))
  (stack-add-cells count t))

(defun stack-grow-down (&optional count)
  "Add COUNT cells to the bottom of the stack, COUNT will defaul to 1."
  (interactive)
  (if (eq count nil)
      (setq count 1))
  (stack-add-cells count nil))

(defun stack-init ()
  (interactive)
  ;; First, check to see if the buffer is empty.
  (save-excursion
    (end-of-buffer)
    (if (> (point) 1)
        (error "Buffer is not empty %s" (point))))
  ;; Now create the starting cell.
  (stack-insert-line ?^ t)
  (stack-insert-line ?-)
  (stack-insert-line ? )
  (stack-insert-line ?-)
  (stack-insert-line ?v t)
  ;; Now extend the stack down.
  (stack-add-cells 40 nil)
  (beginning-of-buffer)
  (next-line 20)
  )

(defun stack-clear-offsets ()
  "Remove all the stack offsets"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (not (looking-at "^$"))
      (progn
        (delete-char stack-left-whitespace)
        (stack-insert-leading-whitespace)
        (forward-line 1)))))
 
(defun stack-write-cell-offset (offset &optional hex)
  "Write the stack offset on to the current line.  Will check that the
current line is a cell boundary line, otherwise an error will be thrown.
Ensures that the offset does not flow the available space."
  (if (< stack-left-whitespace 4)
      (error "Not enough space to insert stack offsets"))

  (save-excursion
    (beginning-of-line)
    (forward-char stack-left-whitespace)
    (if (not (looking-at-p "\|-+\|"))
        (error "Not a cell boundary line"))
    (beginning-of-line)
    (let* ((sym (cond ((< offset 0) ?-)
                      ((> offset 0) ?+)
                      (t ? )))
           (str (format (if hex "%c0x%x " "%c%d ") sym (abs offset)))
           (fmt (format "%%-%ds" stack-left-whitespace)))
      (if (> (length str) stack-left-whitespace)
          (setq str "..."))
      (delete-char stack-left-whitespace)
      (insert (format fmt str)))))

(defun stack-mark-offsets (&optional hex)
  "Add stack offsets onto the stack picture.  Starting at the current line.
Not complete yet.  Should offer more control, and should try to
align the offsets to cell boundaries.

If HEX is true then the offsets are displayed in hex, otherwise
they will be displayed in decimal"
  (interactive)
  (stack-clear-offsets)
  ;; Save current position and then add offsets down.
  (ignore-errors
    (save-excursion
      (stack-mode-align-to-cell)
      (let ((offset 0)
            (stop-at (save-excursion
                       (stack-find-stack nil)
                       (point))))
        (beginning-of-line)        
        (while (not (eq (point) stop-at))
          (progn
            (stack-write-cell-offset offset hex)
            (setq offset (- offset stack-cell-bytes))
            (forward-line 2))))))

  ;; Save current position and then add offset upwards.
  (ignore-errors
    (save-excursion
      (stack-mode-align-to-cell)
      (let ((offset 0)
            (stop-at (save-excursion
                       (stack-find-stack t)
                       (point))))
        (while (not (eq (point) stop-at))
          (progn
            (forward-line -2)
            (setq offset (+ offset stack-cell-bytes))
            (stack-write-cell-offset offset hex)))))))

(defun stack-mode-align-to-cell ()
  "Move point to the start of the line that is the lower edge of the
current stack cell.  Will given an error if the stack structure does
not appear correct."
  (interactive)
  (beginning-of-line)
  (forward-char stack-left-whitespace)
  (if (not (looking-at-p "\|[-v]+\|"))
      (forward-line 1)
    (beginning-of-line)))

(defun stack-mode-current-cell-location ()
  "Return a list describing the location of the current stack
cell.  The list is a list has the form (TOP BOTTOM LEFT RIGHT)
where each of TOP, BOTTOM, LEFT, RIGHT is of the form (BEG . END)
where BEG is the character position for the first character of
that component of the cell, and END is the position for the last
character of the cell."
  (save-excursion
    (stack-mode-align-to-cell)
    (let ((bottom (cons (+ (point) stack-left-whitespace)
                        (+ (point) stack-left-whitespace
                           (stack-cell-char-width) 2)))
          (top (progn
                 (forward-line -2)
                 (cons (+ (point) stack-left-whitespace)
                       (+ (point) stack-left-whitespace
                          (stack-cell-char-width) 2))))
          (left (progn
                  (forward-line 1)
                  (cons (+ (point) stack-left-whitespace)
                        (+ (point) stack-left-whitespace 1))))
          (right (cons (+ (point) stack-left-whitespace
                          1 (stack-cell-char-width))
                       (+ (point) stack-left-whitespace
                          2 (stack-cell-char-width)))))
      (list top bottom left right))))

(defvar stack-current-cell-overlay nil)

(defun stack-mode-highlight-current-cell ()
  "Highlight the cell the cursor is current on."
  (interactive)
  (save-excursion
    ;; Unhighlight previous cell.
    (if stack-current-cell-overlay
        (mapcar (lambda (o) 
                  (delete-overlay o)) stack-current-cell-overlay))
    (let* ((loc (stack-mode-current-cell-location))
           (ovly (mapcar (lambda (l)
                           (let* ((beg (car l))
                                 (end (cdr l))
                                 (o (make-overlay beg end)))
                             (overlay-put o 'face 'font-lock-preprocessor-face)
                             o))
                         loc)))
      (setq stack-current-cell-overlay ovly))
))

(run-with-idle-timer 1 t (lambda () (interactive)
                           (if (eq major-mode 'stack-mode)
                               (stack-mode-highlight-current-cell))))

;; The stack-mode stuff.

(defvar stack-mode-hook nil)

(defvar stack-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c u") 'stack-grow-up)
    (define-key map (kbd "C-c d") 'stack-grow-down)
    (define-key map (kbd "C-c o") 'stack-mark-offsets)
    (define-key map (kbd "C-c O") (lambda () (interactive) 
                                    (stack-mark-offsets t)))
    map)
  "Keymap for stack major mode")

(defun stack-mode ()
  "Major mode for creating stack models"
  (interactive)
  (kill-all-local-variables)
  (use-local-map stack-mode-map)
  (setq major-mode 'stack-mode)
  (setq mode-name "STACK")
  (run-hooks 'stack-mode-hook))

(provide 'stack-mode)

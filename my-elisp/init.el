;; ===========================================================================
;; Turn off the damn splash screen.
(setq inhibit-splash-screen t)

;; ===========================================================================
;; Set the default face to something reasonable, not great, but 
;; it'll do for now until I can figure out something better.
(if (display-graphic-p)
    (set-face-attribute 'default nil :font "Monospace-12"))

;; ===========================================================================
;; Add extra directories to the load path.
(defconst elisp-directory (expand-file-name "~/.emacs.d/"))

(add-to-list 'load-path elisp-directory)
(add-to-list 'load-path (concat elisp-directory "/completion-ui"))
(add-to-list 'load-path (concat elisp-directory "/ess-5.7.1/lisp"))
(add-to-list 'load-path (concat elisp-directory "/cc-mode-5.31.3"))
(add-to-list 'load-path (concat elisp-directory "/color-theme"))
(add-to-list 'load-path (concat elisp-directory "/yasnippet"))
(add-to-list 'load-path (concat elisp-directory "/cedet"))
(add-to-list 'load-path (concat elisp-directory "/site-specific"))

(if (>= emacs-major-version 24)
    (add-to-list 'load-path (concat elisp-directory "/cperl-mode")))

;; ===========================================================================
;; Allow auto complete mode to be toggled on and off. This uses features that
;; are only available on version 23 of emacs (or above).
(if (eq emacs-major-version 23)
    (progn 
      (add-to-list 'load-path (concat elisp-directory "/auto-complete"))
      (autoload 'auto-complete-mode "auto-complete")
      (global-set-key (kbd "C-x A") 'auto-complete-mode)))

;; ===========================================================================
;; Setup initial frame parameters. Remembering to set for the initial frame
;; as well as all further default frames. The call to modify all frames
;; causes any currently displayed frames to be updated immediately.
(setq default-frame-alist `((width . 80) 
                            (height . 40) 
                            (menu-bar-lines . 0)
                            (tool-bar-lines . 0)
                            (background-color . "black") 
                            (foreground-color . "grey")))

(setq initial-frame-alist default-frame-alist)

(modify-all-frames-parameters default-frame-alist)

;; ===========================================================================
; Set this up so that we can tell if we are using xemacs.
;;(defconst using-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(defun using-xemacs ()
  (string-match "XEmacs\\|Lucid" emacs-version))

;;============================================================================
;; Give me more room to type. Turn off menu and tool bar.
(require 'menu-toggle)
(global-set-key [f12] 'toggle-menubar-and-toolbar)

;; ===========================================================================
;; Load undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; ===========================================================================
;; Allow blocks to be hidden / shown on demand.
(require 'hideshow)

;; ===========================================================================
;; For doing haskell - will fail silently if haskell mode is not available on
;; the machine that this emacs is being run on.
(require 'haskell-mode nil t)

;; ===========================================================================
;; Better buffer list.
(global-set-key "\C-x\C-b" 'electric-buffer-list)

(setq buffer-menu-buffer-font-lock-keywords
      '(("^....[*]Man .*Man.*"   . font-lock-variable-name-face) ;Man page
        (".*Dired.*"             . font-lock-comment-face)       ; Dired
        ("^....[*]shell.*"       . font-lock-preprocessor-face)  ; shell buff
        (".*[*]scratch[*].*"     . font-lock-function-name-face) ; scratch buffer
        ("^....[*].*"            . font-lock-string-face)        ; "*" named buffers
        ("^..[*].*"              . font-lock-constant-face)      ; Modified
        ("^.[%].*"               . font-lock-keyword-face)))     ; Read only

(defun buffer-menu-custom-font-lock  ()
  (let ((font-lock-unfontify-region-function
         (lambda (start end)
           (remove-text-properties start end '(font-lock-face nil)))))
    (font-lock-unfontify-buffer)
    (set (make-local-variable 'font-lock-defaults)
         '(buffer-menu-buffer-font-lock-keywords t))
    (font-lock-fontify-buffer)))

(add-hook 'electric-buffer-menu-mode-hook 'buffer-menu-custom-font-lock)

;(setq inhibit-startup-buffer-menu t)
(defun list-buffers (&optional files-only)
  (electric-buffer-list files-only))

;;============================================================================
;; Setup font lock mode.
(defun andrew-configure-font-lock-faces ()
  ;; Red comments when running in an xterm.
  (if (not (display-graphic-p))      
      (set-face-attribute 'font-lock-comment-face nil
                          :foreground "red1"))
  
  (set-face-attribute 'font-lock-type-face nil
                      :foreground "medium purple")
  
  (set-face-attribute 'font-lock-builtin-face nil
                      :foreground "orange red"))

(defun andrew-configure-font-lock-mode ()
  (setq font-lock-maximum-decoration  t
        font-lock-verbose             t
        font-lock-support-mode        'jit-lock-mode
        lazy-lock-defer-on-scrolling  nil
        lazy-lock-defer-contextually  t
        lazy-lock-stealth-verbose     t
        lazy-lock-stealth-lines       50
        lazy-lock-stealth-time        3)
  (andrew-configure-font-lock-faces))

(add-hook 'font-lock-mode-hook 'andrew-configure-font-lock-mode)
(global-font-lock-mode t)

;;===========================================================================
;; Set up the spell checker to use.
;; The alternative is ispell - but aspell gives better suggestions.
(setq-default ispell-program-name "aspell")
;;
;; Make sure that we pick up the correct dictionary name.
;; In truth only the ispell-dictionary needs to be set, but
;; it is nice to set them both just to be on the safe side.
;;
(if (string-equal ispell-program-name "aspell")
  ;; aspell has 'british' dictionary.
  (progn (setq flyspell-default-dictionary "british")
         (setq ispell-dictionary "british"))
  ;;
  ;; ispell calls it an english dictionary.
  (progn (setq flyspell-default-dictionary "english")
         (setq ispell-dictionary "english")))

;;============================================================================
;; Highlight bracket to bracket.
;;   Use a different hightlight style when running in terminal mode as the
;;   number of available colours is less, and we can't do strike through.
(show-paren-mode t)

(if (display-graphic-p)
    (progn
      (setq show-paren-style (quote expression))
      (set-face-attribute 'show-paren-match-face nil :background "grey25")
      (set-face-attribute 'show-paren-mismatch-face nil :background "purple1")
      (set-face-attribute 'show-paren-mismatch-face nil :strike-through "red"))
    (progn
      (setq show-paren-style (quote parenthesis))
      (set-face-attribute 'show-paren-match-face nil :background "green")
      (set-face-attribute 'show-paren-mismatch-face nil :background "red")))

;;============================================================================
;; Highlight selected region.
(transient-mark-mode t)

;;============================================================================
;; Truncate lines, as opposed to wrapping them.
;; This is only a pain when doing emails.
(set-default `truncate-lines t)

(defun set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
		      (window-start (selected-window)))))

;;============================================================================
;; Tab indents, not insert tab.
(set-default `indent-tabs-mode nil)

;;============================================================================
;; Show line numbers in the task bar area.
(setq line-number-mode t)
(setq column-number-mode t)

;;============================================================================
;; Get the email from the environment, and warn me when this doesn't work.
(if (not (getenv "EMAIL"))
    (progn
      (display-warning 
       :warning 
       "Missing EMAIL environment variable, setting email to <invalid>.")
      (setq user-mail-address "<invalid>")))

;;============================================================================
;; This will configure some faces for me. The faces don't exists in
;; normal mode, so calling set-face-attribute will fail. HOWEVER; the
;; call to lambda says: delay evaluation till run time, in which case
;; flyspell mode (for example) will have been loaded, and the face will
;; then exist - magic :)
;;
;; Faces for flyspell-mode
(add-hook 'flyspell-mode-hook
          (lambda()
            (set-face-attribute 'flyspell-duplicate-face nil
                                :foreground "DeepSkyBlue1"
                                :underline t
                                :weight 'bold)))

;;============================================================================
;; For some reason C-<home> and C-<end> don't work inside a terminal.
(global-set-key (kbd "C-x <home>") 'beginning-of-buffer)
(global-set-key (kbd "C-x <end>") 'end-of-buffer)
(global-unset-key (kbd "C-<home>"))
(global-unset-key (kbd "C-<end>"))

;; ===========================================================================
;; First, make sure we use cperl mode not perl mode.
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;; A bug in emacs 24 causes the following line to be needed.
;; Would be nice to remove this one day.
(if (>= emacs-major-version 24)
    (progn
      (defvar cperl-invalid-face nil)
      (setq cperl-highlight-variables-indiscriminately 't)))

;; Now load my cperl customisations when entering cperl-mode.
(autoload 'andrew-cperl-mode "andrew-cperl-mode")
(add-hook 'cperl-mode-hook 'andrew-cperl-mode)

;;============================================================================
;; Load my prefered C / C++ style
(autoload 'andrew-cc-mode "andrew-cc-mode")
(add-hook 'c-mode-hook 'andrew-cc-mode)
(add-hook 'c++-mode-hook 'andrew-cc-mode)

;;============================================================================
;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;;============================================================================
;; Set the frame title to display the full path name.
(if (using-xemacs)
    (setq frame-title-format "xemacs: %f") ;; xemacs title
    (setq frame-title-format "emacs: %f")) ;; emacs title

;;============================================================================
(setq minibuffer-max-depth nil)

;; ===========================================================================
;; Turn off the scroll bar - mouse button works!
(set-scroll-bar-mode nil)

;;============================================================================
;; Jump to matching parenthesis.
(autoload 'jump-to-matching-paren "mparen")
(global-set-key (kbd "C-x #") 'jump-to-matching-paren)

;;============================================================================
;; Bind the goto line function.
(global-set-key [?\M-g] 'goto-line)

;;============================================================================
;; Function for moving through the windows backwards
(defun other-window-backward ()
  "Like other window, but go backwards."
  (interactive)
  (other-window -1))
(global-set-key (kbd "C-x O") 'other-window-backward)

;;============================================================================
;;(setq font-lock-auto-fontify t)
;(setq vc-handle-cvs nil)
(setq vc-handled-backends nil)
(setq version-control 'never)

;;============================================================================
;; File templates.
;; The templates are found in my ~/.templates directory.
;; The disable regexp will stop somefiles from having header lines 
;; updated when the buffer is killed.
(require 'template)
(template-initialize)
(setq template-auto-update-disable-regexp ".ido.last")
(setq template-auto-update nil)

;;============================================================================
;; My first elisp function...
(defun get-pod (buf)
  "Runs pod2text on the saved version of a buffer."
  (interactive "bBuffer Name: ")
  (let ((target-buffer (get-buffer-create "*__pod__*")))
    (shell-command (format "perldoc -t %s" (buffer-file-name (get-buffer buf)))
                   target-buffer)))

;;============================================================================
;; Allow typed text to overwrite a highlighted area
(delete-selection-mode t)

;;============================================================================
;; Stuff to do in latex mode.
(add-hook 'latex-mode-hook
          '(lambda ()
             (auto-fill-mode 1)
             (flyspell-mode 1)
	     (setq LaTeX-figure-label "figure:")
	     (setq LaTeX-table-label "table:")))

;;============================================================================
;; Setting this to nil will mean that backup files, releated to
;; CVS checked out files get a version number associated with
;; them. I don't want this, so I turn it off, I just get the
;; default <filename>~ backup files.
(setq vc-make-backup-files t)

;;============================================================================
;; Make scripts executable after they have been saved.
(add-hook 'after-save-hook
   '(lambda ()
      (let ( (temp (substring buffer-file-name -3)) )
        (if (or (equal temp ".pl")
                (equal temp ".sh"))
            (executable-make-buffer-file-executable-if-script-p)))))

;;============================================================================
;; Allow the current line to be highlighted.
(make-face 'hl-line)
(set-face-attribute 'hl-line nil
                    :background "gray25")

(global-set-key (kbd "C-x C-h") 'hl-line-mode)

;;============================================================================
;; Allow line numbers in the buffer.
(global-set-key (kbd "C-x N") 'global-linum-mode)

;;============================================================================
;; Allow me to grow/shrink the window (when spilt horizontally)
;; from the keyboard.
(global-set-key (kbd "C-x <up>") 'enlarge-window)
(global-set-key (kbd "C-x <down>") 'shrink-window)

;;============================================================================
;; Have the pointer move away from the cursor.
(if (display-graphic-p)
    (mouse-avoidance-mode 'cat-and-mouse))

;;============================================================================
;; Change the cursor colour when we enter insert mode.

(defvar cursor-default-colour "LimeGreen")
(defvar cursor-overwrite-colour "red")

(defun cursor-overwrite-mode ()
  "Set cursor colour according to insert mode"
  (set-cursor-color
   (if overwrite-mode
       cursor-overwrite-colour
       cursor-default-colour)))
(add-hook 'post-command-hook 'cursor-overwrite-mode)

;; ===========================================================================
;; When the mark is activated change the cursor shape - this would be nice
;; but does not work. In c-mode if the mark has been used at all then 
;; using the electric paren stuff causes the activate mark hook to fire.
(defun cursor-activate-mark ()
  (progn
    (setq cursor-type 'bar)))
(add-hook 'activate-mark-hook 'cursor-activate-mark)

(defun cursor-deactivate-mark ()
  (progn
    (setq cursor-type 'box)))
(add-hook 'deactivate-mark-hook 'cursor-deactivate-mark)

;;============================================================================
;; Function stolen from will - thanks :) - that will convert dos carriage
;; return characters into nothing - dos what the dos2unix command line does.

(defun dos2unix () 
  "Convert dos formatted buffer to unix formatted buffer by removing\nany \\r characters."
  (interactive)
  (let ((current-point-position (point)))
    (progn 
      (message "dos2unix: Converting dos characters '\r' to ''")
      (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match ""))
      (goto-char current-point-position)))
      (message "dos2unix: done."))

;;============================================================================
;; Printing configuration...
(setq lpr-command "a2ps")
(setq lpr-switches '("--sides=duplex" "-r" "--columns=2"))
(setq printer-name "HPLaserJet4050")
(setq lpr-add-switches nil)

;;============================================================================
;; Take care of supporting R mode.
(autoload 'r-mode "ess-site")
(add-to-list 'auto-mode-alist '("\\.R\\'" . r-mode))

;;============================================================================
;; I create gdb command scripts as *.gdb
(add-to-list 'auto-mode-alist '("\\.gdb\\'" . gdb-script-mode))

;;============================================================================
;; For javascript mode
(add-hook 'javascript-mode-hook 
          '(lambda () (setq js-indent-level 2)) )
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist (cons  "\\.js\\'" 'javascript-mode))

;;============================================================================
;; Lets make grep better
(require 'grep)
(grep-apply-setting 'grep-command "grep --exclude='*~' --exclude='.#*' -IHn -e ")

;;============================================================================
;; Support for forth syntax highlighting.
(add-hook 'forth-mode-hook
	  '(lambda ()
	     (setq forth-indent-level 2)
             (setq forth-minor-indent-level 1)))

(let ( (GFORTH_EL "gforth.el") )
  (setq forth-custom-words  '( (("QT\"") compile-only (font-lock-string-face . 1)
                                "[\"\n]" nil string   (font-lock-string-face . 1))) )
  (autoload 'forth-mode GFORTH_EL)
  (add-to-list 'auto-mode-alist '("\\.of\\'"  . forth-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'"  . forth-mode))
  (add-to-list 'auto-mode-alist '("\\.fs\\'"  . forth-mode))
  (add-to-list 'auto-mode-alist '("\\.fth\\'" . forth-mode)))

;;============================================================================
;; Support for git syntax highlighting.
(autoload 'git-commit-mode "git-commit-mode.el")
(add-to-list 'auto-mode-alist '("\\.git/COMMIT_EDITMSG\\'"  . git-commit-mode))

;; ===========================================================================
;; Linker script mode.
(require 'ld-script)

;; ===========================================================================
;; Interactive selection from the kill ring
(require 'browse-kill-ring)
(global-set-key "\C-cy" 'browse-kill-ring)

;; temporarily highlight the inserted `kill-ring' entry
(setq browse-kill-ring-highlight-inserted-item t)

;; string separating entries in the `separated' style
(setq browse-kill-ring-separator
      "\n--separator------------------------------")

;; face in which to highlight the `browse-kill-ring-separator'
(defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
(setq browse-kill-ring-separator-face 'separator-face)

(set-face-attribute 'highlight nil
                    :background "gray25")

;; highlight the current entry in the kill ring window
(setq browse-kill-ring-highlight-current-entry t)

;; Restore window configuration when closing kill ring buffer.
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Don't allow real cursor movement in the kill ring window, instead
;; just jump between entries. This keeps the highlighting correct too.
(define-key browse-kill-ring-mode-map (kbd "<down>") 'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map (kbd "<up>") 'browse-kill-ring-previous)
(define-key browse-kill-ring-mode-map (kbd "<right>") 'browse-kill-ring-forward)
(define-key browse-kill-ring-mode-map (kbd "<left>") 'browse-kill-ring-previous)

;; ===========================================================================
;; Save bookmarks everytime I change one.
(setq bookmark-save-flag 1)

;; ===========================================================================
;; Make diff mode a little prettier
(defun andrew-configure-diff-mode ()  
  (set-face-attribute 'diff-file-header nil
                      :foreground "green"
                      :background "unspecified")

  (set-face-attribute 'diff-header nil
                      :foreground "steel blue"
                      :background "unspecified")

  (set-face-attribute 'diff-function nil
                      :foreground "red2"
                      :background "unspecified")

  (set-face-attribute 'diff-refine-change nil
                      :foreground "black"
                      :background "pale turquoise")
  )

(add-hook 'diff-mode-hook 'andrew-configure-diff-mode)

;; ===========================================================================
(require 'winpoint)
(window-point-remember-mode 1)

;; ===========================================================================
;; Rectangle marking
(define-key ctl-x-map "r\C-@" 'rm-set-mark)
(define-key ctl-x-map [?r ?\C-\ ] 'rm-set-mark)
(define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
(define-key ctl-x-map "r\C-w" 'rm-kill-region)
(define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)

(autoload 'rm-set-mark "rect-mark" "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark" "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark" "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark" "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark" "Drag out a rectangular region with the mouse." t)

;; ===========================================================================
;; Special mode for editing CGEN .cpu files, basically scheme mode with
;; some extra highlighting thrown in.
(autoload 'cpu-mode "cpu-mode")
(add-to-list 'auto-mode-alist '("\\.cpu\\'" . cpu-mode))

;; ===========================================================================
;; Completion suggestions in the mini-buffer. 
(require 'icomplete+)
(icomplete-mode)

;; ===========================================================================
;; Enable some disabled commands that are pretty USEFUL.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ===========================================================================
;; Mark the fill column using a thin line, setup key combo to toggle mode.
(require 'fill-column-indicator)
(setq-default fill-column 75)
(global-set-key (kbd "C-x |") 'fci-mode)

;; ===========================================================================
;;       Color Theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn (color-theme-initialize)))

;; ===========================================================================
;;       Org Mode
(add-to-list 'load-path (concat elisp-directory "/org-mode/lisp"))
(add-to-list 'load-path (concat elisp-directory "/org-mode/contrib/lisp"))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

(add-hook 'org-mode-hook 
          (lambda () 
            (auto-fill-mode 1)
            (flyspell-mode 1)))

(defun org-file (name)
  (let ((org-directory (expand-file-name "~/.org/")))
    (concat org-directory name)))

(setq org-agenda-files (list (org-file "/")))

(setq org-default-notes-file (org-file "refile.org"))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 1))))

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; ===========================================================================
;; Load site specific initialisaion - don't error if it can't be found.
(require 'site-init nil t)

;; ===========================================================================
;; Hints & Tips
;; ============
;;
;; Regexp matching for "[" or "]" in a character set: [][]

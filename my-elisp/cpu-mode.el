(require 'scheme)

;; These are the CGEN convenience macros.
(setq cgen-convenience-keywords '("dnop" "dnf" "df" "dni" 
                                  "define-full-operand"))

;; These are all the CGEN keywords of the form "(define-<FOO>"
(setq cgen-define-keywords '("arch" "isa" "cpu" "mach" "model" 
                             "hardware" "keyword" "ifield"
                             "multi-ifield" "attr" "normal-insn-enum"
                             "operand" ))

(define-derived-mode cpu-mode scheme-mode "CGEN"
  "A major mode for editing CGEN cpu files."
  
  (let ((initial-scheme-font-lock-keywords-2 scheme-font-lock-keywords-2))
    (setq scheme-font-lock-keywords-2
          (append initial-scheme-font-lock-keywords-2
                  (list 
                   (list (concat "(\\(define-"
                                 (regexp-opt cgen-define-keywords)
                                 "\\)") 
                         '(1 font-lock-keyword-face))
                   (list (concat "(\\(define-pmacro\\)\\s-+(\\(\\sw+\\)")
                         '(1 font-lock-keyword-face)
                         '(2 font-lock-function-name-face))
                   (list (concat "(\\(include\\)\\s-+")
                         '(1 font-lock-preprocessor-face))
                   (list (concat "(\\(" (regexp-opt cgen-convenience-keywords) "\\)"
                                 "\\s-+\\(\\sw+\\)\\>")
                         '(1 font-lock-keyword-face)
                         '(2 font-lock-variable-name-face))
                   ))))
  )

(provide 'cpu-mode)



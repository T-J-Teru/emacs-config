#! /projects/firepath_work/aburgess/software/.install/bin/guile-1.8.8 \
-s
!#

;; The main procedure
(define (main args)
  (display "Hello World\nCommand Line: ")
  (display args)
  (newline)
  (>>>POINT<<<)
)

;; The first thing that actually gets run.
(main (command-line))

#!/bin/env -S guile -e main
!#

(use-modules (statprof)
             (dsv))

(define (main args)
  (statprof (lambda ()
              (with-output-to-port (%make-void-port "w")
                (lambda ()
                  (scm->dsv (make-list 10000 '("a" "b" "c" "d"))
                            #:format 'unix))))
            #:count-calls? #t))

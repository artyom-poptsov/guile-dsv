#!/bin/env -S guile -e main
!#

(use-modules (statprof)
             (dsv))

(define (main args)
  (statprof (lambda ()
              (dsv-string->scm
               (string-join (make-list 10000 "a,b") "\n")
               #:format 'unix))
            #:count-calls? #t))

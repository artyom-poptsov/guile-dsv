#!/bin/env -S guile -e main
!#

(use-modules (statprof)
             (dsv))

(define (main args)
  (statprof (lambda ()
              (with-output-to-port (%make-void-port "w")
                (lambda ()
                  (scm->dsv (make-list 1000000 (make-list 10 "a\nb"))
                            #:format 'rfc4180))))
            #:count-calls? #t))

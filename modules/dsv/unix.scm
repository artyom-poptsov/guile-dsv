;;; rfc4180.scm -- DSV parser for Unix format.

;; Copyright (C) 2015-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; The program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with the program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; A parser for Unix-style of DSV data format as described in
;; <http://www.catb.org/~esr/writings/taoup/html/ch05s02.html#id2901882>


;;; Code:

(define-module (dsv unix)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi  srfi-1)
  #:use-module (srfi  srfi-26)
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (dsv common)
  #:use-module (dsv builder)
  #:use-module (dsv fsm unix)
  #:use-module (dsv fsm unix-writer)
  #:use-module (dsv fsm unix-writer-with-fibers)
  #:use-module (dsv fsm context)
  #:use-module (dsv fsm dsv-context)
  #:autoload (fibers) (run-fibers)
  #:export (make-builder
            dsv->scm
            dsv-string->scm
            scm->dsv
            scm->dsv-string
            guess-delimiter
            ;; Variables
            %default-delimiter))

(define-with-docs %default-delimiter
  "Default delimiter for DSV"
  #\:)

(define-with-docs %default-comment-prefix
  "Default comment prefix for DSV"
  #\#)

(define-with-docs %default-line-break
  "Default line break for DSV"
  "\n")

(define-with-docs %char-mapping
  "Characters to substitute in the output Unix format data.  Each character in
the hash table keys must be replaced with its substitution character, preceded
by the escape symbol."
  (alist->hash-table
   '((#\page    . #\f)
     (#\newline . #\n)
     (#\return  . #\r)
     (#\tab     . #\t)
     (#\vtab    . #\v)
     (#\\       . #\\))))



(define* (dsv->scm port
                   #:key
                   (debug-mode? #f)
                   (delimiter %default-delimiter)
                   (validate? #f)
                   (comment-prefix %default-comment-prefix))
  (let* ((fsm (make <unix-fsm>
                #:pre-action pre-action
                #:debug-mode? debug-mode?))
         (context (fsm-run! fsm
                            (make-char-context
                             #:port port
                             #:debug-mode? debug-mode?
                             #:custom-data (alist->hash-table
                                            `((delimiter . ,(if (equal? delimiter 'default)
                                                                %default-delimiter
                                                                delimiter))
                                              (validate? . ,validate?)
                                              (comment-prefix . ,(if (equal? comment-prefix 'default)
                                                                     %default-comment-prefix
                                                                     comment-prefix))))))))
    (context-result context)))

(define* (dsv-string->scm str
                          #:key
                          (debug-mode? #f)
                          (validate? #f)
                          (delimiter %default-delimiter)
                          (comment-prefix %default-comment-prefix))
  (call-with-input-string str
    (lambda (port)
      (dsv->scm port
                #:debug-mode?    debug-mode?
                #:delimiter      delimiter
                #:validate?      validate?
                #:comment-prefix comment-prefix))))


(define (make-builder input-data port delimiter line-break)
  (%make-builder input-data
                 port
                 'unix
                 (value-or-default delimiter  %default-delimiter)
                 (value-or-default line-break %default-line-break)
                 %char-mapping))

(define* (scm->dsv builder
                   #:key
                   (debug-mode? #f)
                   (log-driver  "null")
                   (log-opt     '()))

  (smc-log-init! log-driver log-opt)

  (let* ((fibers-module (resolve-module '(fibers)
                                        #:ensure #f))
         (fsm (if fibers-module
                  (make <unix-writer-with-fibers-fsm>
                    #:pre-action unix-writer-update
                    #:debug-mode? debug-mode?)
                  (make <unix-writer-fsm>
                    #:pre-action unix-writer-update
                    #:debug-mode? debug-mode?)))
         (proc (lambda ()
                 (fsm-run! fsm
                           (make-unix-writer
                            (builder-input-data builder)
                            #:debug-mode? debug-mode?
                            #:port (builder-port builder)
                            #:delimiter (string (builder-delimiter builder))
                            #:char-mapping %char-mapping
                            #:line-break (builder-line-break builder))))))
    (if fibers-module
        (run-fibers proc)
        (proc))))

(define (scm->dsv-string scm delimiter line-break)
  (call-with-output-string
   (lambda (port)
     (scm->dsv (make-builder scm port delimiter line-break)))))


(define guess-delimiter (make-delimiter-guesser dsv-string->scm 'unix))

;;; unix.scm ends here

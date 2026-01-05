;;; rfc4180.scm -- DSV parser for RFC 4180 format.

;; Copyright (C) 2015-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; A parser of RFC 4180 <https://tools.ietf.org/html/rfc4180> data format
;; (Comma-Separated Values, CSV).


;;; Code:

(define-module (dsv rfc4180)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (ice-9 rdelim)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (dsv common)
  #:use-module (dsv builder)
  #:use-module (dsv fsm rfc4180)
  #:use-module (dsv fsm rfc4180-writer)
  #:use-module (dsv fsm rfc4180-writer-with-fibers)
  #:use-module (dsv fsm context)
  #:use-module (dsv fsm dsv-context)
  #:autoload (fibers) (run-fibers)
  #:export (make-builder
            scm->dsv
            scm->dsv-string
            dsv->scm
            dsv-string->scm
            guess-delimiter
            make-special-character-set
            ;; Variables
            %default-delimiter))


;;; Global variables

(define-with-docs %default-line-break
  "Default line break style as described in the RFC."
  "\r\n")

(define-with-docs %default-delimiter
  "Default field delimiter."
  #\,)

(define-with-docs %char-mapping
  "Characters to substitute in the output RFC41890 format data.  Each character
in the hash table keys must be replaced with its substitution character,
preceded by the escape symbol."
  (alist->hash-table
   '((#\" . #\"))))


;;; Writing

(define (make-builder scm port delimiter line-break)
  (%make-builder scm
                 port
                 'rfc4180
                 (value-or-default delimiter  %default-delimiter)
                 (value-or-default line-break %default-line-break)
                 %char-mapping))

(define (make-special-character-set delimiter)
  (char-set delimiter #\" #\newline))

(define* (scm->dsv builder
                   #:key
                   (debug-mode? #f)
                   (log-driver  "null")
                   (log-opt     '()))
  "Create a DSV document from a data."

  (smc-log-init! log-driver log-opt)

  (let* ((fibers-module (resolve-module '(fibers)
                                        #:ensure #f))
         (fsm (if fibers-module
                  (make <rfc4180-writer-with-fibers-fsm>
                    #:pre-action dsv-context-update
                    #:debug-mode? debug-mode?)
                  (make <rfc4180-writer-fsm>
                    #:pre-action dsv-context-update
                    #:debug-mode? debug-mode?)))
         (proc (lambda ()
                 (fsm-run! fsm
                           (make-dsv-context
                            (builder-input-data builder)
                            #:debug-mode? debug-mode?
                            #:port (builder-port builder)
                            #:delimiter (string (builder-delimiter builder))
                            #:char-mapping %char-mapping
                            #:line-break (builder-line-break builder)
                            #:custom-data (make-special-character-set (builder-delimiter builder)))))))
    (if fibers-module
        (run-fibers proc)
        (proc))))

(define (scm->dsv-string scm delimiter line-break)
  (call-with-output-string
   (lambda (port)
     (scm->dsv (make-builder scm port delimiter line-break)))))


;;;; The parser itself.

;; XXX: The procedure does not handle comments.  Although the RFC 4180 says
;; nothing about comments inside CSV data, it might be useful to handle
;; comments in some way if it is explicitly requested by the user.
(define* (dsv->scm port
                   #:key
                   (debug-mode? #f)
                   (validate? #f)
                   (delimiter %default-delimiter))
  (let* ((fsm (make <rfc4180-fsm>
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
                                              (validate?      . ,validate?)
                                              (comment-prefix . #f)))))))
    (context-result context)))

(define* (dsv-string->scm str
                          #:key
                          (debug-mode? #f)
                          (validate?   #f)
                          (delimiter %default-delimiter))
  (call-with-input-string str
    (lambda (port)
      (dsv->scm port
                #:debug-mode?    debug-mode?
                #:validate?      validate?
                #:delimiter      delimiter))))

(define guess-delimiter (make-delimiter-guesser dsv-string->scm 'rfc4180))

;;; rfc4180.scm ends here

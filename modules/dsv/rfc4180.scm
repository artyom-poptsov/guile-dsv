;;; rfc4180.scm -- DSV parser for RFC 4180 format.

;; Copyright (C) 2015-2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module (ice-9 hash-table)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (dsv common)
  #:use-module (dsv fsm rfc4180)
  #:use-module (dsv fsm rfc4180-writer)
  #:use-module (dsv fsm rfc4180-writer-with-fibers)
  #:use-module (dsv fsm context)
  #:use-module (dsv fsm dsv-context)
  #:autoload (fibers) (run-fibers)
  #:export (scm->dsv
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

(define (make-special-character-set delimiter)
  (char-set delimiter #\" #\newline))

(define* (scm->dsv table
                   #:key
                   (delimiter %default-delimiter)
                   (line-break %default-line-break)
                   (port (current-output-port))
                   (debug-mode? #f)
                   (log-driver  "null")
                   (log-opt     '()))
  "Create a DSV document from a data."

  (smc-log-init! log-driver log-opt)

  (let* ((delimiter  (value-or-default delimiter  %default-delimiter))
         (line-break (value-or-default line-break  %default-line-break))
         (fibers-module (resolve-module '(fibers)
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
                            table
                            #:debug-mode? debug-mode?
                            #:port port
                            #:delimiter (string delimiter)
                            #:char-mapping %char-mapping
                            #:line-break line-break
                            #:custom-data (make-special-character-set delimiter))))))
    (if fibers-module
        (run-fibers proc)
        (proc))))

(define* (scm->dsv-string table
                          #:key
                          (delimiter %default-delimiter)
                          (line-break %default-line-break)
                          (port (current-output-port))
                          (debug-mode? #f)
                          (log-driver  "null")
                          (log-opt     '()))
  (call-with-output-string
   (lambda (port)
     (scm->dsv table
               #:delimiter   delimiter
               #:line-break  line-break
               #:port        port
               #:debug-mode? debug-mode?
               #:log-driver  log-driver
               #:log-opt     log-opt))))


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

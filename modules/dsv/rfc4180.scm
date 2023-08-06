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
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (ice-9 rdelim)
  #:use-module (scheme documentation)
  #:use-module (oop goops)
  #:use-module (dsv common)
  #:use-module (dsv parser)
  #:use-module (dsv builder)
  #:use-module (dsv fsm rfc4180)
  #:use-module (dsv fsm context)
  #:export (make-parser
            make-string-parser
            make-builder
            scm->dsv
            scm->dsv-string
            dsv->scm
            dsv-string->scm
            guess-delimiter
            ;; Variables
            %default-delimiter))


;;; Global variables

(define-with-docs %default-line-break
  "Default line break style as described in the RFC."
  "\r\n")

(define-with-docs %default-delimiter
  "Default field delimiter."
  #\,)


;;; Writing

(define (make-builder scm port delimiter line-break)
  (%make-builder scm
                 port
                 'rfc4180
                 (value-or-default delimiter  %default-delimiter)
                 (value-or-default line-break %default-line-break)))


(define (escape-double-quotes field)
  "Escape each double-quote in a FIELD with additional double-quote."
  (escape-special-chars field #\" #\"))

(define (quote-field field)
  "Quote a FIELD with double-quotes."
  (string-append (string #\") field (string #\")))

(define* (scm->dsv builder)
  "Create a DSV document from a data using a BUILDER."

  (define (should-be-enclosed? field)
    "Check if a FIELD should be enclosed in double-quotes."
    (or (string-index field    (char-set (builder-delimiter builder)
                                         #\" #\newline))
        (string-contains field (builder-line-break builder))))

  (builder-build builder
                 (lambda (field)
                   (let ((escaped-field (escape-double-quotes field)))
                     (if (should-be-enclosed? escaped-field)
                         (quote-field escaped-field)
                         field)))))

(define (scm->dsv-string scm delimiter line-break)
  (call-with-output-string
   (lambda (port)
     (scm->dsv (make-builder scm port delimiter line-break)))))


;;; Reading

(define (make-parser port delimiter known-delimiters comment-prefix)
  (%make-parser port
                'rfc4180
                (value-or-default delimiter        %default-delimiter)
                (value-or-default known-delimiters %known-delimiters)
                comment-prefix))

(define (make-string-parser str delimiter known-delimiters comment-prefix)
  (call-with-input-string str (cut make-parser <> delimiter known-delimiters
                                   comment-prefix)))

;; (define (test)
;;   (display (frame-procedure-name (stack-ref (make-stack #t) 1))))


;;;; The parser itself.

;; XXX: The procedure does not handle comments.  Although the RFC 4180 says
;; nothing about comments inside CSV data, it might be useful to handle
;; comments in some way if it is explicitly requested by the user.
(define* (dsv->scm port
                   #:key
                   (debug-mode? #f)
                   (delimiter %default-delimiter))
  (let* ((fsm (make <rfc4180-fsm>
                #:debug-mode? debug-mode?))
         (context (fsm-run! fsm
                            (make-char-context
                             #:port port
                             #:debug-mode? debug-mode?
                             #:custom-data `((delimiter . ,(if (equal? delimiter 'default)
                                                               %default-delimiter
                                                               delimiter))
                                             (comment-prefix . #f))))))
    (context-result context)))

(define* (dsv-string->scm str
                          #:key
                          (debug-mode? #f)
                          (delimiter %default-delimiter))
  (call-with-input-string str
    (lambda (port)
      (dsv->scm port
                #:debug-mode?    debug-mode?
                #:delimiter      delimiter))))

(define guess-delimiter (make-delimiter-guesser dsv-string->scm 'rfc4180))

;;; rfc4180.scm ends here

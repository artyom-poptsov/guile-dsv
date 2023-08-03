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
  #:use-module (dsv common)
  #:use-module (dsv parser)
  #:use-module (dsv builder)
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


;;; Helper procedures

(define-syntax case-pred
  (syntax-rules (else)
    ((_ pred key ((datum ...) exp) ...)
     (cond
      ((or (pred key datum) ...) exp) ...))
    ((_ pred key ((datum ...) exp) ... (else else-exp ...))
     (cond
      ((or (pred key datum) ...) exp) ...
      (else else-exp ...)))))


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
(define (dsv->scm parser)
  (define (fsm-read-quote-crlf table row buffer)
    (define %current-state 'read-quote-crlf)
    (let ((char (parser-read-char parser)))
      (cond
       ((linefeed? char)
        (fsm-read (append table (list (append row (list (buffer->string buffer)))))
                  '()                   ; row
                  '()))                 ; buffer
       (else
        (dsv-error %current-state
                   "Missing line feed after carriage return"
                   `((state  . ,%current-state)
                     (table  . ,table)
                     (row    . ,row)
                     (buffer . ,buffer)
                     (char   . ,char)))))))

  (define (fsm-read-quote table row buffer)
    (define %current-state 'read-quote)
    (let ((char (parser-read-char parser)))
      (cond
       ((double-quote? char)
        (debug-fsm-transition %current-state 'read-quoted-field)
        (fsm-read-quoted-field table row (cons char buffer)))
       ((delimiter? parser char)
        (debug-fsm-transition %current-state 'read)
        (fsm-read table
                  (append row (list (buffer->string buffer)))
                  '()))
       ((carriage-return? char)
        (fsm-read-quote-crlf table row buffer))
       ((linefeed? char)
        (debug-fsm-transition %current-state 'read)
        (fsm-read (append table (list (append row (if (null? buffer)
                                                      (list "")
                                                      (list (buffer->string buffer))))))
                  '()                   ; row
                  '()))                 ; buffer
       ((eof-object? char)
        (debug-fsm-transition %current-state 'end 'final)
        (append table (list (append row (if (null? buffer)
                                            (list "")
                                            (list (buffer->string buffer)))))))
       (else
        (dsv-error %current-state
                   "A field contains unescaped double-quotes"
                   `((state  . ,%current-state)
                     (table  . ,table)
                     (row    . ,row)
                     (buffer . ,buffer)
                     (char   . ,char)))))))

  (define (fsm-read-quoted-field table row buffer)
    (define %current-state 'read-quoted-field)
    (let ((char (parser-read-char parser)))
      (cond
       ((eof-object? char)
        (dsv-error 'fsm-read-quoted-field
                   "Missing quote at the end of a quoted field"
                   `((state  . ,%current-state)
                     (table  . ,table)
                     (row    . ,row)
                     (buffer . ,buffer)
                     (char   . ,char))))
       ((double-quote? char)
        (debug-fsm-transition %current-state 'read-quote)
        (fsm-read-quote table row buffer))
       (else
        (fsm-read-quoted-field table row (cons char buffer))))))

  (define (fsm-read-field-crlf table row buffer)
    (define %current-state 'read-field-crlf)
    (let ((char (parser-read-char parser)))
      (cond
       ((linefeed? char)
        (fsm-read (append table (list (append row (list (buffer->string buffer)))))
                  '()                   ; row
                  '()))                 ; buffer
       (else
        (dsv-error %current-state
                   "Missing line feed after carriage return"
                   `((state  . ,%current-state)
                     (table  . ,table)
                     (row    . ,row)
                     (buffer . ,buffer)
                     (char   . ,char)))))))

  (define (fsm-read-field table row buffer)
    (define %current-state 'read-field)
    (let ((char (parser-read-char parser)))
      (cond
        ((or (eof-object? char) (delimiter? parser char))
         (debug-fsm-transition %current-state 'read)
         (fsm-read table
                   (append row (list (buffer->string buffer))) ; row
                   '()))                                       ; buffer
        ((carriage-return? char)
         (fsm-read-field-crlf table row buffer))
        ((linefeed? char)
         (debug-fsm-transition %current-state 'read)
         (fsm-read (append table (list (append row (list (buffer->string buffer)))))
                   '()                   ; row
                   '()))                 ; buffer
        ((double-quote? char)
         (dsv-error %current-state "A double quote inside an unquoted field"
                    `((table  . ,table)
                      (row    . ,row)
                      (buffer . ,buffer)
                      (char   . ,char))))
        (else
         (fsm-read-field table row (cons char buffer))))))

  (define (fsm-read table row buffer)
    (define %current-state 'read)
    (let ((char (parser-read-char parser)))
      (cond
        ((eof-object? char)
         (debug-fsm-transition %current-state 'end 'final)
         (if (null? row)
             table
             (append table (list row))))
        ((carriage-return? char)
         (fsm-read table row buffer))
        ((double-quote? char)
         (debug-fsm-transition %current-state 'read-quoted-field)
         (fsm-read-quoted-field table row buffer))
        ((delimiter? parser char)
         (fsm-read table (append row (list "")) '()))
        ((linefeed? char)
         (if (and (null? buffer) (not (null? row)))
             (fsm-read (append table (list (append row (list ""))))
                       '()
                       '())
             (fsm-read (append table (list row))
                       '()                  ; row
                       '())))               ; buffer
        (else
         (debug-fsm-transition %current-state 'read-field)
         (fsm-read-field table row (cons char buffer))))))
    (fsm-read '() '() '()))

(define (make-delimiter-guesser parser-proc)
  (lambda (parser)
    "Guess a DSV string delimiter."
    (and (> (length (parser-known-delimiters parser)) 1)
         (let* ((get-length (lambda (d)
                              (let ((parser (set-delimiter parser d)))
                                (seek (parser-port parser) 0 SEEK_SET)
                                (catch #t
                                  (lambda () (length (car (parser-proc parser))))
                                  (const 0)))))
                (delimiter-list (map (lambda (d) (cons d (get-length d)))
                                     (parser-known-delimiters parser)))
                (guessed-delimiter-list
                 (fold (lambda (a prev)
                         (if (not (null? prev))
                             (let ((a-count (cdr a))
                                   (b-count (cdar prev)))
                               (cond ((> a-count b-count) (list a))
                                     ((= a-count b-count) (append (list a)
                                                                  prev))
                                     (else prev)))
                             (list a)))
                       '()
                       delimiter-list)))
           (and (= (length guessed-delimiter-list) 1)
                (caar guessed-delimiter-list))))))

(define guess-delimiter (make-delimiter-guesser dsv->scm))

;;; rfc4180.scm ends here

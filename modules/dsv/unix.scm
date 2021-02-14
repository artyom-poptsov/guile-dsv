;;; rfc4180.scm -- DSV parser for RFC 4180 format.

;; Copyright (C) 2015, 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module (srfi  srfi-1)
  #:use-module (srfi  srfi-26)
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (scheme documentation)
  #:use-module (dsv common)
  #:use-module (dsv parser)
  #:use-module (dsv builder)
  #:export (make-parser
            make-string-parser
            make-builder
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
  "#")

(define-with-docs %default-line-break
  "Default line break for DSV"
  "\n")

(define-with-docs %char-mapping
  "Nonprintable characters."
  '((#\page    . "\\f")
    (#\newline . "\\n")
    (#\return  . "\\r")
    (#\tab     . "\\t")
    (#\vtab    . "\\v")))


(define (make-parser port delimiter known-delimiters comment-prefix)
  (%make-parser port
                'unix
                (value-or-default delimiter        %default-delimiter)
                (value-or-default known-delimiters %known-delimiters)
                (value-or-default comment-prefix   %default-comment-prefix)))

(define (make-string-parser str delimiter known-delimiters comment-prefix)
  (call-with-input-string str (cut make-parser <> delimiter known-delimiters
                                   comment-prefix)))


(define (linefeed? char)
  (and (char? char) (char=? char #\newline)))

(define (carriage-return? char)
  (and (char? char) (char=? char #\return)))

(define (delimiter? parser char)
  (and (char? char) (char=? char (parser-delimiter parser))))

(define (buffer->string buffer)
  (list->string (reverse buffer)))

(define (backslash? char)
  (char=? #\\ char))


(define (dsv->scm parser)
  (define (fsm-error state message table row buffer)
    (dsv-error %current-state
               message
               `((state  . ,state)
                 (table  . ,table)
                 (row    . ,row)
                 (buffer . ,buffer)
                 (char   . ,char))))

  (define (fsm-append-row table row buffer)
    (define %current-state 'append-row)
    (debug-fsm-transition %current-state 'read)
    (fsm-read (append table (list (append row (list (buffer->string buffer)))))
              '()
              '()))

  (define (fsm-append-field table row buffer)
    (define %current-state 'append-field)
    (debug-fsm-transition %current-state 'read)
    (fsm-read table (append row (list (buffer->string buffer))) '()))

  (define (fsm-append-last-field table row buffer)
    (define %current-state 'append-last-field)
    (debug-fsm-transition %current-state 'return-result)
    (if (null? buffer)
        (fsm-return-result table row '())
        (fsm-return-result table
                           (append row (list (buffer->string buffer)))
                           '())))

  (define (fsm-return-result table row buffer)
    (define %current-state 'return-result)
    (debug-fsm-transition %current-state 'end 'final)
    ;; (format #t "table: ~a; row: ~a; buffer: ~a~%" table row buffer)
    (if (null? row)
        table
        (append table (list row))))

  (define (fsm-read-escaped-char table row buffer)
    (define %current-state 'read-escaped-char)
    (let ((char (parser-read-char parser)))
      (debug-fsm-transition %current-state 'read)
      (cond
       ((eof-object? char)
        (fsm-error %current-state
                   "EOF escaped with a backslash"
                   table row buffer))
       ((char=? char #\n)
        (fsm-read table row (cons #\newline buffer)))
       ((char=? char #\t)
        (fsm-read table row (cons #\tab buffer)))
       ((char=? char #\v)
        (fsm-read table row (cons #\vtab buffer)))
       ((char=? char #\r)
        (fsm-read table row (cons #\return buffer)))
       ((char=? char #\f)
        (fsm-read table row (cons #\page buffer)))
       ((or (linefeed? char) (carriage-return? char))
            (fsm-read table row buffer))
       (else
        (fsm-read table row (cons char buffer))))))

  (define (fsm-skip-comment table row buffer)
    (define %current-state 'skip-comment)
    (let ((char (parser-read-char parser)))
      (cond
       ((or (carriage-return? char) (linefeed? char))
        (debug-fsm-transition %current-state 'read)
        (fsm-read table row buffer))
       (else
        (fsm-skip-comment table row buffer)))))

  (define (fsm-read table row buffer)
    (define %current-state 'read)
    (let ((char (parser-read-char parser)))
      (cond
       ((eof-object? char)
        (debug-fsm-transition %current-state 'append-last-field)
        (fsm-append-last-field table row buffer))
       ((parser-comment-prefix? parser (string char))
        ;; TODO: Improve comment prefix detection
        (debug-fsm-transition %current-state 'skip-comment)
        (fsm-skip-comment table row buffer))
       ((or (carriage-return? char)  (linefeed? char))
        (debug-fsm-transition %current-state 'append-row)
        (fsm-append-row table row buffer))
       ((delimiter? parser char)
        (debug-fsm-transition %current-state 'append-field)
        (fsm-append-field table row buffer))
       ((backslash? char)
        (debug-fsm-transition %current-state 'read-escaped-char)
        (fsm-read-escaped-char table row buffer))
       (else
        (fsm-read table row (cons char buffer))))))

  (fsm-read '() '() '()))


(define (make-builder input-data port delimiter line-break)
  (%make-builder input-data
                 port
                 'unix
                 (value-or-default delimiter  %default-delimiter)
                 (value-or-default line-break %default-line-break)))

(define (serialize-nonprintable-chars str)
  "Replace nonprintable characters with C-style backslash escapes."
  (let subst ((s    str)
              (lst  %char-mapping))
    (if (not (null? lst))
        (let ((s (regexp-substitute/global #f (string (caar lst)) s
                                           'pre (cdar lst) 'post)))
          (subst s (cdr lst)))
        s)))

(define (escape builder str)
  "Escape special characters with a backslash."
  (escape-special-chars (escape-special-chars str #\\ #\\)
                        (builder-delimiter builder)
                        #\\))

(define* (scm->dsv builder)
  (builder-build builder
                 (lambda (field)
                   (serialize-nonprintable-chars (escape builder field)))))

(define (scm->dsv-string scm delimiter line-break)
  (call-with-output-string
   (lambda (port)
     (scm->dsv (make-builder scm port delimiter line-break)))))


(define guess-delimiter (make-delimiter-guesser dsv->scm))

;;; unix.scm ends here

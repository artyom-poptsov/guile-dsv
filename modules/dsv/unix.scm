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

(define (deserealize-nonprintable-chars str)
  "Replace C-style backslash escapes with actual chars."
  (let subst ((s   str)
              (lst %char-mapping))
    (if (not (null? lst))
        (let ((s (substitute s
                             (string-append "\\" (cdar lst))
                             (string (caar lst)))))
          (subst s (cdr lst)))
        s)))

(define (unescape-backslash str)
  (substitute str "\\\\\\\\" "\\"))

(define (unsescape parser str)
  (unescape-backslash
   (unescape-chars str (parser-delimiter parser) #\\)))

(define (deserealize parser str)
  (unsescape parser (deserealize-nonprintable-chars str)))


(define (string-split/escaped str delimiter)
  "Split a string STR into the list of the substrings delimited by appearances
of the DELIMITER.

This procedure is simlar to string-split, but works correctly with
escaped delimiter -- that is, skips it.  E.g.:

  (string-split/escaped \"car:cdr:ca\\:dr\" #\\:)
  => (\"car\" \"cdr\" \"ca\\:dr\")
"
  (let ((fields (string-split str delimiter)))
    (fold (lambda (field prev)
            (if (and (not (null? prev))
                     (string-suffix? "\\" (last prev)))
                (append (drop-right prev 1)
                        (list (string-append
                               (string-drop-right (last prev) 1)
                               (string delimiter)
                               field)))
                (append prev (list field))))
          '()
          fields)))


(define* (splice lst-1 lst-2 #:optional (delimiter ""))
  (cond
   ((null? lst-1)
    lst-2)
   ((null? lst-2)
    lst-1)
   ((and (null? lst-1) (null? lst-2))
    '())
   (else
    (append (drop-right lst-1 1)
            (list (string-append (string-drop-right (last lst-1) 1)
                                 delimiter
                                 (car lst-2)))
            (drop lst-2 1)))))

(define (dsv->scm parser)
  (define (fsm-read-ln dsv-list buffer)
    (debug-fsm-transition 'read-ln)
    (let ((line (parser-read-line parser)))
      (if (eof-object? line)
          (fsm-end dsv-list)
          (cond
           ((parser-commented? parser line)
            (debug-fsm 'read-ln "the line is commented out~%")
            (debug-fsm-transition 'read-ln 'read-ln)
            (fsm-read-ln dsv-list buffer))
           (else
            (let ((rec (string-split/escaped line (parser-delimiter parser))))
              (cond
               ((string-suffix? "\\" (last rec))
                (debug-fsm-transition 'read 'read-ln)
                (fsm-read-ln dsv-list (splice buffer rec)))
               (else
                (debug-fsm-transition 'read-ln 'deserealize)
                (fsm-deserealize dsv-list (splice buffer rec))))))))))
  (define (fsm-deserealize dsv-list buffer)
    (let ((buffer (map (cut deserealize parser <>) buffer)))
      (debug-fsm-transition 'deserealize 'add)
      (fsm-add dsv-list buffer)))
  (define (fsm-add dsv-list buffer)
    (fsm-read-ln (cons buffer dsv-list) '()))
  (define (fsm-end dsv-list)
    (reverse dsv-list))
  (fsm-read-ln '() '()))


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

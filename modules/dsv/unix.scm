;;; rfc4180.scm -- DSV parser for RFC 4180 format.

;; Copyright (C) 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define (decerealize parser str)
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
  (let fold-file ((dsv-list '())
                  (buffer   '())
                  (state    'read-ln))
    (debug-fsm-transition state)
    (case state
      ((read-ln)
       (debug-fsm state "dsv-list: ~s; buffer: ~s~%" dsv-list buffer)
       (let ((line (parser-read-line parser)))
         (debug-fsm state "line: ~s~%" line)
         (cond
          ((not (eof-object? line))
           (cond
            ((parser-commented? parser line)
             (debug-fsm state "the line is commented out~%")
             (debug-fsm-transition state state)
             (fold-file dsv-list buffer state))
            (else
             (let ((rec (string-split/escaped line (parser-delimiter parser))))
               (cond
                ((string-suffix? "\\" (last rec))
                 (debug-fsm-transition state 'read-ln)
                 (fold-file dsv-list (splice buffer rec) 'read-ln))
                (else
                 (debug-fsm-transition state 'decerealize)
                 (fold-file dsv-list (splice buffer rec) 'decerealize)))))))
          (else
           (debug-fsm-transition state 'end)
           (fold-file dsv-list buffer 'end)))))

      ((decerealize)
       (let ((buffer (map (cut decerealize parser <>) buffer)))
         (debug-fsm-transition state 'add)
         (fold-file dsv-list buffer 'add)))

      ((add)
       (debug-fsm-transition state 'read-ln)
       (fold-file (cons buffer dsv-list) '() 'read-ln))

      ((end)
       (debug-fsm-transition state 'STOP 'final)
       (reverse dsv-list)))))


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

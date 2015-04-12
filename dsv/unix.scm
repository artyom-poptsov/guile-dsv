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


;;; Code:

(define-module (dsv unix)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi  srfi-1)
  #:use-module (srfi  srfi-26)
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (scheme documentation)
  #:use-module (dsv common)
  #:use-module (dsv parser)
  #:export (make-parser
            make-string-parser
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


(define (make-parser port delimiter known-delimiters comment-prefix)
  (%make-parser port
                'unix
                (if (eq? delimiter 'default)
                    %default-delimiter
                    delimiter)
                (if (eq? known-delimiters 'default)
                    %known-delimiters
                    known-delimiters)
                (if (eq? comment-symbol 'default)
                    %default-comment-prefix
                    comment-prefix)))

(define (make-string-parser str delimiter known-delimiters comment-prefix)
  (call-with-input-string str (cut make-parser <> delimiter known-delimiters
                                   comment-prefix)))


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


(define (dsv->scm parser)

  (define (commented? line)
    "Check if the LINE is commented."
    (string-prefix? (parser-comment-prefix parser) (string-trim line)))

  (let parse ((dsv-list '())
              (line     (parser-read-line parser)))
    (if (not (eof-object? line))
        (if (not (commented? line))
            (parse (cons (string-split/escaped line (parser-delimiter parser))
                         dsv-list)
                   (parser-read-line parser))
            (parse dsv-list (parser-read-line parser)))
        (reverse dsv-list))))

(define* (scm->dsv scm port delimiter #:key (line-break %default-line-break))

  (define (->dsv lst)
    (string-join (map (cut escape-special-chars <> delimiter #\\)
                      lst)
                 (string delimiter)))

  (for-each (cut format port "~a~a" <> line-break)
            (map ->dsv scm)))

(define* (scm->dsv-string lst delimiter #:key (line-break %default-line-break))
  (call-with-output-string
   (cut scm->dsv lst <> delimiter #:line-break line-break)))


(define guess-delimiter (make-delimiter-guesser dsv->scm))

;;; unix.scm ends here

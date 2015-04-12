;;; parser.scm -- A DSV parser.

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

(define-module (dsv parser)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 rdelim)
  #:use-module (scheme documentation)
  #:export (<dsv-parser>
            %make-parser
            make-delimiter-guesser
            dsv-parser?
            parser-port
            parser-type
            parser-delimiter
            parser-comment-prefix
            parser-read-line
            parser-string-split
            parser-delimiter->string
            parser-comment-symbol->string
            parser-commented?

            ;; Variables
            %known-delimiters))

(define-with-docs %known-delimiters
  "List of known delimiters"
  '(#\, #\: #\; #\| #\tab #\space))

(define-immutable-record-type <dsv-parser>
  (%make-parser port type delimiter known-delimiters comment-prefix)
  dsv-parser?
  (port             parser-port)
  (type             parser-type)
  (delimiter        parser-delimiter set-delimiter)
  (known-delimiters parser-known-delimiters)
  (comment-prefix   parser-comment-prefix))         ; string | 'none

(set-record-type-printer!
 <dsv-parser>
 (lambda (parser port)
   "Print information about a PARSER to a PORT."
   (format port "#<dsv-parser port: ~a type: ~a delim: ~s ~a>"
           (parser-port      parser)
           (parser-type      parser)
           (parser-delimiter parser)
           (number->string (object-address parser) 16))))

(define (parser-read-line parser)
  (read-line (parser-port parser)))

(define (parser-string-split parser str)
  (string-split str (parser-delimiter parser)))

(define (parser-delimiter->string parser)
  (string (parser-delimiter parser)))

(define (parser-commented? parser str)
  "Check if a string STR is commented."
  (and (not (eq? (parser-comment-prefix parser) 'none))
       (string-prefix? (parser-comment-prefix parser) (string-trim str))))

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

;;; parser.scm ends here

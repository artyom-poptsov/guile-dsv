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
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 rdelim)
  #:export (<dsv-parser>
            %make-parser
            dsv-parser?
            parser-port
            parser-type
            parser-delimiter
            parser-comment-symbol
            parser-read-line
            parser-string-split
            parser-delimiter->string
            parser-comment-symbol->string))

(define-record-type <dsv-parser>
  (%make-parser port type delimiter comment-symbol)
  dsv-parser?
  (port           parser-port)
  (type           parser-type)
  (delimiter      parser-delimiter)
  (comment-symbol parser-comment-symbol))

(define (parser-read-line parser)
  (read-line (parser-port parser)))

(define (parser-string-split parser str)
  (string-split str (parser-delimiter parser)))

(define (parser-delimiter->string parser)
  (string (parser-delimiter parser)))

(define (parser-comment-symbol->string parser)
  (string (parser-comment-symbol parser)))

;;; parser.scm ends here

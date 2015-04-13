;;; builder.scm -- A DSV builder.

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

(define-module (dsv builder)
  #:use-module (srfi srfi-9 gnu)
  #:export (<dsv-builder>
            %make-builder
            dsv-builder?
            builder-input-data
            builder-port
            builder-delimiter
            builder-line-break
            builder-display
            builder-join
            builder-build))

(define-immutable-record-type <dsv-builder>
  (%make-builder input-data port type delimiter line-break)
  dsv-builder?
  (input-data builder-input-data)       ; list
  (port       builder-port)             ; port
  (type       builder-type)             ; symbol
  (delimiter  builder-delimiter)        ; char
  (line-break builder-line-break))      ; string

(set-record-type-printer!
 <dsv-builder>
 (lambda (builder port)
   "Print information about a BUILDER to a PORT."
   (format port "#<dsv-builder port: ~a type: ~a delim: ~s ~a>"
           (builder-port builder)
           (builder-type builder)
           (builder-delimiter builder)
           (number->string (object-address builder) 16))))

(define (builder-display builder line)
  "Display a LINE to the BUILDER output port."
  (let ((p (builder-port builder)))
    (display line p)
    (display (builder-line-break builder) p)))

(define (builder-join builder lst)
  "Join a list LST of strings using BUILDER delimiter."
  (string-join lst (string (builder-delimiter builder))))

(define (builder-build builder proc)
  "Build a DSV data using a BUILDER.  Apply PROC on each field of each row,
print data to the BUILDER port."
  (for-each (lambda (row)
              (builder-display builder (builder-join builder (map proc row))))
            (builder-input-data builder)))

;;; builder.scm ends here

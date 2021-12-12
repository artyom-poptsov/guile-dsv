;;; common.scm -- Common Guile-DSV CLI code.

;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Common Guile-DSV CLI code.


;;; Code:

(define-module (dsv cli common)
  #:export (string->dsv-format))

(define (string->dsv-format str)
  "Convert a string STR to a DSV format type."
  (let ((fmt (string->symbol str)))
    (case fmt
      ((unix rfc4180)
       fmt)
      (else
       (error "Wrong format" str)))))

;;; common.scm ends here.

;;; cli.scm -- Tests for Guile-DSV CLI code.

;; Copyright (C) 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (dsv cli common))


(test-begin "cli")

(test-equal "string->dsv-format: unix"
  'unix
  (string->dsv-format "unix"))

(test-equal "string->dsv-format: rfc4180"
  'rfc4180
  (string->dsv-format "rfc4180"))

(test-error "string->dsv-format: error"
  (string->dsv-format "wrong-format"))

(test-equal "remove-empty-rows"
  '(("a" "b")
    ("c" "d"))
  (remove-empty-rows '(("a" "b")
                       ("")
                       ("c" "d"))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "cli")

(exit (zero? exit-status))

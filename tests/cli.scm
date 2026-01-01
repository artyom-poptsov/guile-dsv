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

(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (tests test)
             (dsv cli common))


(define %test-name "cli")


(test-begin %test-name)
(configure-test-logging! %test-name)

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

(test-equal "borders->alist: empty spec"
  '()
  (borders->alist ""))

(test-equal "borders->alist: list of two borders specifications"
  '((border-top  . "|")
    (border-top-left . "+"))
  (borders->alist "bt=|,btl=+"))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))

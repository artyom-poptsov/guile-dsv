;;; common.scm -- Tests for Guile-DSV common code.

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

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (dsv)
             (dsv common))


(test-begin "common")

(test-equal "value-or-default: value"
  'some-value
  (value-or-default 'some-value 'other-value))

(test-equal "value-or-default: default"
  'other-value
  (value-or-default 'default 'other-value))



(test-assert "linefeed?"
  (and (linefeed? #\newline)
       (not (linefeed? #\return))
       (not (linefeed? "not a char"))))

(test-assert "carriage-return?"
  (and (carriage-return? #\return)
       (not (carriage-return? #\newline))
       (not (carriage-return? "not a CR"))))

(test-assert "double-quote?"
  (and (double-quote? #\")
       (not (double-quote? #\'))))

(test-assert "backslash?"
  (and (backslash? #\\ )
       (not (backslash? #\/))))



(test-equal "buffer->string"
  "hello"
  (buffer->string '(#\o #\l #\l #\e #\h)))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "common")

(exit (zero? exit-status))

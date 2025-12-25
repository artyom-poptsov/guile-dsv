;;; table-preset.scm -- Tests for table presets.

;; Copyright (C) 2021-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (dsv table-preset))

(define %test-name "table-preset")

(test-begin %test-name)


(test-assert "table-preset-name?: #t"
  (table-preset-name? "ascii"))

(test-assert "table-preset-name?: #f"
  (not (table-preset-name? "rs=-,cs=|")))



(define %table-presets-path
  (format #f "~a/presets" (getenv "abs_top_srcdir")))

(test-assert "print-table-presets"
  (with-output-to-string
    (lambda ()
      (print-table-presets (current-output-port)
                           #:table-presets-path %table-presets-path))))

(test-equal "table-preset-override"
  '((a . b) (c . d))
  (table-preset-override '((a . e) (c . d))
                         '((a . b))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))

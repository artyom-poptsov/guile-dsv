;;; table.scm -- Tests for parsed DSV tables.

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
             (dsv table))

(define %test-name "table")

(test-begin %test-name)


(test-equal "get-width"
  '(1 2 3)
  (let ((t (dsv-string->scm "a,b,c\n1,12,123\n" #\,)))
    (get-width t)))

(test-equal "shorthand->table-parameter"
  'border-top
  (shorthand->table-parameter 'bt))



(test-assert "format-table"
  (let ((table (dsv-string->scm "a,b,c\n1,12,123\n" #\,)))
    (with-output-to-string
      (lambda ()
        (format-table table
                      '((border-top          . "-")
                        (border-top-left     . ".")
                        (border-top-right    . ".")
                        (border-top-joint    . "-")
                        (border-left         . "|")
                        (border-left-joint   . "|")
                        (border-right        . "|")
                        (border-right-joint  . "|")
                        (row-separator       . "-")
                        (row-joint           . "+")
                        (column-separator    . "|")
                        (border-bottom       . "-")
                        (border-bottom-left  . "'")
                        (border-bottom-right . "'")
                        (border-bottom-joint . "-")
                        ;; Header style.
                        (header-top              . "-")
                        (header-top-left         . ".")
                        (header-top-right        . ".")
                        (header-top-joint        . "-")
                        (header-left             . "|")
                        (header-right            . "|")
                        (header-bottom           . "=")
                        (header-bottom-left      . "|")
                        (header-bottom-right     . "|")
                        (header-bottom-joint     . "+")
                        (header-column-separator . "|")))))))

  
(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))

;;; table.scm ends here.

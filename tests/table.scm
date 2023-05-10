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



(test-equal "filter-row"
  '(("a1" "b1" "c1")
    ("a3" "b3" "c3"))
  (let ((table (dsv-string->scm "a1,b1,c1\na2,b2,c2\na3,b3,c3\n" #\,)))
    (table-filter-row (lambda (value row) (not (= row 1))) table)))

(test-equal "filter-column"
  '(("a1" "c1")
    ("a2" "c2")
    ("a3" "c3"))
  (let ((table (dsv-string->scm "a1,b1,c1\na2,b2,c2\na3,b3,c3\n" #\,)))
    (table-filter-column (lambda (value col) (not (= col 1))) table)))

(test-equal "table-format-field"
  ;; .--------------------------------------------------------.
  ;; | space      | purpose              | value              |
  ;; |============+======================+====================|
  ;; | 1          | field separator      | " "                |
  ;; |------------+----------------------+--------------------|
  ;; | 10         | field space          | "a          "      |
  ;; |------------+----------------------+--------------------|
  ;; | 5          | padding              | "     "            |
  ;; |------------+----------------------+--------------------|
  ;; | 1          | field separator      | " "                |
  ;; '--------------------------------------------------------'
  " a               "
  (table-format-field "a"
                      10
                      #:padding 5))

(test-equal "string*"
  "aaaaa"
  (string* "a" 5))

(test-equal "stylize: string"
  "[31;1mtest[0m"
  (stylize "test" "31;1"))

(test-equal "stylize: #f"
  "test"
  (stylize "test" #f))

(test-equal "table-print-element: string"
  "#"
  (with-output-to-string
    (lambda ()
      (table-print-element "#" (current-output-port)))))

(test-equal "table-print-element: #f"
  " "
  (with-output-to-string
    (lambda ()
      (table-print-element #f (current-output-port)))))

(test-equal "get-width"
  '(1 2 3)
  (get-width '(("a" "b"  "c")
               ("d" "ee" "f")
               ("g" "i"  "jjj"))))



(test-equal "string-slice"
  '("aa" "aa" "a")
  (string-slice "aaaaa" 2))

(test-equal "table-wrap-row"
  '(("aa" "aa" "a") ("bb" "bb"))
  (let ((row '("aaaaa" "bbbb")))
    (table-wrap-row row '(2 2))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))

;;; table.scm ends here.

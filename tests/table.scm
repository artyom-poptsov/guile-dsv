;;; table.scm -- Tests for parsed DSV tables.

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


(add-to-load-path (getenv "abs_top_srcdir"))

(use-modules (srfi srfi-64)
             (srfi srfi-26)
             (tests test)
             (dsv)
             (dsv table)
             (dsv table-preset))

(define %test-name "table")

(test-begin %test-name)
(configure-test-logging! %test-name)


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

(test-equal "format-table: with shadow"
  (string-join
   (list
    ".--------------.  "
    "| a1 | b1 | c1 |  "
    "|----+----+----|.."
    "| a2 | b2 | c2 |.."
    "|----+----+----|.."
    "| a3 | b3 | c3 |.."
    "|----+----+----|.."
    "| a4 | b4 | c4 |.."
    "'--------------'.."
    "  ................"
    "")
   "\n")
  (let* ((table '(("a1" "b1" "c1")
                  ("a2" "b2" "c2")
                  ("a3" "b3" "c3")
                  ("a4" "b4" "c4")))
         (presets-path (string-append (getenv "abs_top_srcdir")
                                      "/presets/"))
         (preset (load-table-preset "ascii"
                                    #:table-presets-path presets-path))
         (preset (table-preset-override preset
                                        '((shadow . ".")
                                          (shadow-offset . "2;1")))))
    (with-output-to-string
      (lambda ()
        (format-table table preset)))))

(test-equal "format-table: with shadow and header"
  (string-join
   (list
    ".--------------.  "
    "| a1 | b1 | c1 |  "
    "|====+====+====|.."
    "| a2 | b2 | c2 |.."
    "|----+----+----|.."
    "| a3 | b3 | c3 |.."
    "|----+----+----|.."
    "| a4 | b4 | c4 |.."
    "'--------------'.."
    "  ................"
    "")
   "\n")
  (let* ((table '(("a1" "b1" "c1")
                  ("a2" "b2" "c2")
                  ("a3" "b3" "c3")
                  ("a4" "b4" "c4")))
         (presets-path (string-append (getenv "abs_top_srcdir")
                                      "/presets/"))
         (preset (load-table-preset "ascii"
                                    #:table-presets-path presets-path))
         (preset (table-preset-override preset
                                        '((shadow . ".")
                                          (shadow-offset . "2;1")))))
    (with-output-to-string
      (lambda ()
        (format-table table preset #:with-header? #t)))))



(test-equal "smooth: zero elements in a list"
  '()
  (smooth '()))

(test-equal "smooth: one element in a list"
  '(1)
  (smooth '(1)))

(test-equal "smooth: two elements in a list"
  '(26.0 26.0)
  (smooth '(42 10)))



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

(test-equal "string*: zero multiplier"
  ""
  (string* "a" 0))

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
  ""
  (with-output-to-string
    (lambda ()
      (table-print-element #f (current-output-port)))))

(test-equal "get-width"
  '(1 2 3)
  (get-width '(("a" "b"  "c")
               ("d" "ee" "f")
               ("g" "i"  "jjj"))))



(test-equal "string-slice: string length = 5, width = 2"
  '("aa" "aa" "a")
  (string-slice "aaaaa" 2))

(test-equal "string-slice: string length = 5, width = 10"
  '("aaaaa")
  (string-slice "aaaaa" 10))

(test-equal "string-slice: string length = 5, width = 0"
  '("aaaaa")
  (string-slice "aaaaa" 0))

(test-equal "table-wrap-row"
  '(("aa" "aa" "a") ("bb" "bb"))
  (let ((row '("aaaaa" "bbbb")))
    (table-wrap-row row '(2 2))))

(test-equal "table-wrap"
  '((("aaaaa" "aaaaa") ("bbbbb"))
    (("ccc") ("d")))
  (let ((table '(("aaaaaaaaaa" "bbbbb")
                 ("ccc"   "d"))))
    (table-wrap table
                (get-width table)
                #:width 15)))

(test-equal "table-wrap: Custom string-slice"
  '(("aaaaaaaaaa" "bbbbb")
    ("ccc"        "d"))
  (let ((table '(("aaaaaaaaaa" "bbbbb")
                 ("ccc"   "d"))))
    (table-wrap table
                (get-width table)
                #:width 15
                #:string-slice (lambda (s w)
                                 s))))

(test-equal "table-format-row: simple formatting"
  " a  b  c \n"
  (let ((data '(("a" "b" "c"))))
    (with-output-to-string
      (lambda ()
        (table-format-row (car data)
                          #:cell-widths (get-width data))))))

(test-equal "table-calculate-cell-widths"
  '(25 75)
  (table-calculate-cell-widths 100 '(25.0 75.0)))

(test-equal "sum"
  10
  (sum '(2 8)))



(test-equal "table-number"
  '((" " "0" "1" "2" "3" "4")
    ("0" "a" "b" "c" "d" "e")
    ("1" "f" "g" "i" "j" "k"))
  (table-number '(("a" "b" "c" "d" "e")
                  ("f" "g" "i" "j" "k"))))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end %test-name)

(exit (zero? exit-status))

;;; table.scm ends here.

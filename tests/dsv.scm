;;; dsv.scm -- Tests for DSV parser.

;; Copyright (C) 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (dsv))

(test-begin "dsv")

(test-assert "dsv-string->list"
  (let ((test-string0   "a:b:c")
        (expected-list0 '("a" "b" "c"))
        (test-string1   "a,b,c")
        (expected-list1 '("a" "b" "c"))
        (test-string2   "a:b:")
        (expected-list2 '("a" "b" "")))
    (and (equal? expected-list0 (dsv-string->list test-string0))
         (equal? expected-list1 (dsv-string->list test-string1 #\,))
         (equal? expected-list2 (dsv-string->list test-string2)))))

(test-assert "list->dsv-string"
  (let ((test-list0       '("a" "b" "c"))
        (expected-string0 "a:b:c")
        (expected-string1 "a,b,c")
        (test-list1       '("a" "b" ""))
        (expected-string2 "a:b:"))
    (and (equal? expected-string0 (list->dsv-string test-list0))
         (equal? expected-string1 (list->dsv-string test-list0 #\,))
         (equal? expected-string2 (list->dsv-string test-list1)))))

(test-assert "guess-delimiter"
  (and (equal? #\,     (guess-delimiter "a,b,c"))
       (equal? #\:     (guess-delimiter "a:b:c"))
       (equal? #\tab   (guess-delimiter "a	b	c"))
       (equal? #\space (guess-delimiter "a b c"))
       (equal? #\:     (guess-delimiter "a,b:c:d:e"))
       (equal? #\,     (guess-delimiter "a,b,c,d:e"))))

(test-assert "dsv-read"
  (let ((test-data0     "a:b:c")
        (expected-list0 '(("a" "b" "c")))
        (test-data1     "a\\:b:c")
        (expected-list1 '(("a:b" "c"))))
    (and (equal? expected-list0 (call-with-input-string test-data0
                                  (lambda (p)
                                    (dsv-read p))))
         (equal? expected-list1 (call-with-input-string test-data1
                                  (lambda (p)
                                    (dsv-read p)))))))

(test-end "dsv")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; dsv.scm ends here.
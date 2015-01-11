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
             (srfi srfi-26)
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

(test-assert "dsv-string->list, rfc4180"
  (and (equal? '("a" "b")
               (dsv-string->list "a,b" #\, #:format 'rfc4180))
       (equal? '("" "b")
               (dsv-string->list "\"\",b" #\, #:format 'rfc4180))
       (equal? '(",a" "b")
               (dsv-string->list "\",a\",b" #\, #:format 'rfc4180))
       (equal? '("a," "b")
               (dsv-string->list "\"a,\",b" #\, #:format 'rfc4180))
       (equal? '("a" "b")
               (dsv-string->list "\"a\",b" #\, #:format 'rfc4180))
       (equal? '("a,b" "c")
               (dsv-string->list "\"a,b\",c" #\, #:format 'rfc4180))
       (equal? '("\"a\"" "b")
               (dsv-string->list "\"\"\"a\"\"\",b" #\, #:format 'rfc4180))))

(test-assert "list->dsv-string"
  (let ((test-list0       '("a" "b" "c"))
        (expected-string0 "a:b:c")
        (expected-string1 "a,b,c")
        (test-list1       '("a" "b" ""))
        (expected-string2 "a:b:"))
    (and (equal? expected-string0 (list->dsv-string test-list0))
         (equal? expected-string1 (list->dsv-string test-list0 #\,))
         (equal? expected-string2 (list->dsv-string test-list1)))))

(test-assert "list->dsv-string, rfc4180"
  (and (equal? "a,b\r"
               (list->dsv-string '("a" "b") #\, #:format 'rfc4180))
       (equal? "\"a,b\"\r"
               (list->dsv-string '("a,b") #\, #:format 'rfc4180))
       (equal? "\"\"\"a\"\"\",b\r"
               (list->dsv-string '("\"a\"" "b") #\, #:format 'rfc4180))))

(test-assert "guess-delimiter"
  (and (equal? #\,     (guess-delimiter "a,b,c"))
       (equal? #\:     (guess-delimiter "a:b:c"))
       (equal? #\tab   (guess-delimiter "a	b	c"))
       (equal? #\space (guess-delimiter "a b c"))
       (equal? #\:     (guess-delimiter "a,b:c:d:e"))
       (equal? #\,     (guess-delimiter "a,b,c,d:e"))
       (equal? #f      (guess-delimiter "a,b:c"))))

(test-assert "dsv-read"
  (let ((test-data0     "a:b:c")
        (expected-list0 '(("a" "b" "c")))
        (test-data1     "a\\:b:c")
        (expected-list1 '(("a:b" "c")))
        (test-data2     "a\\,b,c")
        (expected-list2 '(("a,b" "c"))))
    (and (equal? expected-list0 (call-with-input-string test-data0
                                  (cut dsv-read <>)))
         (equal? expected-list1 (call-with-input-string test-data1
                                  (cut dsv-read <>)))
         (equal? expected-list2 (call-with-input-string test-data2
                                  (cut dsv-read <> #\,)))
         ;; Check order of read records
         (equal? '(("1") ("2"))
                 (call-with-input-string
                  "1\n2\n"
                  (cut dsv-read <>)))
         ;; Handling of commented lines
         (equal? '(("a" "b" "c"))
                 (call-with-input-string
                  "# this is a comment\na:b:c\n"
                  (cut dsv-read <>)))
         (equal? '(("a" "b" "c"))
                 (call-with-input-string
                  "; this is a comment\na:b:c\n"
                  (cut dsv-read <> #:comment-symbol #\;))))))

(test-assert "dsv-write"
  (and (string=? "a:b:c\n"
                 (call-with-output-string
                  (cut dsv-write '(("a" "b" "c")) <>)))
       (string=? "a:b:c\nd:e:f\n"
                 (call-with-output-string
                  (cut dsv-write '(("a" "b" "c") ("d" "e" "f")) <>)))))

(test-end "dsv")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; dsv.scm ends here.

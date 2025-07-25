;;; dsv.scm -- Tests for DSV parser.

;; Copyright (C) 2014-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (dsv))


(define %test-name "dsv")

(test-begin %test-name)
(configure-test-logging! %test-name)


;;; dsv->scm

(test-equal "dsv->scm: three fields"
  '(("a" "b" "c"))
  (call-with-input-string "a:b:c\n"
    (cut dsv->scm <>)))

(test-equal "dsv->scm: two fields with an escaped colon"
  '(("a:b" "c"))
  (call-with-input-string "a\\:b:c\n"
    (cut dsv->scm <>)))

(test-equal "dsv->scm: two fields with an escaped comma"
  '(("a,b" "c"))
  (call-with-input-string "a\\,b,c\n"
    (cut dsv->scm <> #\,)))

(test-equal "dsv->scm: two fields, check order"
  '(("1") ("2"))
  (call-with-input-string
      "1\n2\n"
    (cut dsv->scm <>)))

(test-equal "dsv-string->scm: three fields, separated by a colon 1"
  '(("a" "b" "c"))
  (dsv-string->scm "a:b:c\n"))

(test-equal "dsv-string->scm: three fields, separated by a colon 2"
  '(("a" "b" ""))
  (dsv-string->scm "a:b:\n"))

(test-equal "dsv-string->scm: three fields, separated by a comma"
  '(("a" "b" "c"))
  (dsv-string->scm "a:b:c\n"))

;; Handling of commented lines
(test-assert "dsv->scm, comment prefix"
  (and (equal? '(("a" "b" "c"))
               (call-with-input-string
                "# this is a comment\na:b:c\n"
                 (cut dsv->scm <>)))
       (equal? '(("a" "b" "c"))
               (call-with-input-string
                "; this is a comment\na:b:c\n"
                (cut dsv->scm <> #:comment-prefix ";")))
       (equal? '(("# this is a comment"))
               (call-with-input-string
                "# this is a comment"
                (cut dsv->scm <> #:comment-prefix 'none)))))

(test-equal "dsv-string->scm, nonprintable characters"
  '(("\f" "a\nb" "\r" "\t" "\v"))
  (dsv-string->scm "\\f:a\\nb:\\r:\\t:\\v\n"))

(test-equal "dsv-string->scm, backslash unescaping"
  '(("a\\b"))
  (dsv-string->scm "a\\\\b\n"))

(test-equal "dsv-string->scm, record continuation"
  '(("a b" "c"))
  (dsv-string->scm "a \\\nb:c\n"))

(test-error "dsv->scm: Escaped EOF error check"
  'dsv-error
  (dsv-string->scm "a:b:c\\"))

(test-assert "dsv-string->dsv: Inconsistent row length"
  (dsv-string->scm "a,b,c\nd,e\n"
                   #\,
                   #:validate? #f))

(test-error "dsv-string->dsv: Inconsistent row length: error"
  #t
  (dsv-string->scm "a,b,c\nd,e\n"
                   #\,
                   #:validate? #t))


;;; scm->dsv

(test-equal "scm->dsv: one row, three fields separated by a colon"
  "a:b:c\n"
  (call-with-output-string
    (cut scm->dsv '(("a" "b" "c")) <>)))

(test-equal "scm->dsv: two rows, three fields"
  "a:b:c\nd:e:f\n"
  (call-with-output-string
    (cut scm->dsv '(("a" "b" "c") ("d" "e" "f")) <>)))

(test-equal "scm->dsv-string: three fields separated by a colon"
  "a:b:c\n"
  (scm->dsv-string '(("a" "b" "c"))))

(test-equal "scm->dsv-string: three fields separated by a comma, one is empty"
  "a,b,\n"
  (scm->dsv-string '(("a" "b" "")) #\,))

(test-equal "scm->dsv-string: three fields separated by a colon, one is empty"
  "a:b:\n"
  (scm->dsv-string '(("a" "b" ""))))

(test-equal "scm->dsv: nonprintable characters"
  "\\f:a\\nb:\\r:\\t:\\v\n"
  (scm->dsv-string '(("\f" "a\nb" "\r" "\t" "\v"))))

(test-equal "scm->dsv: backslash escaping"
  "a\\\\b\n"
  (scm->dsv-string '(("a\\b"))))

(test-equal "scm->dsv-string: RFC4180"
  "The,\"quick,\",brown\r\nfox,jumps,over\r\nthe,lazy,dog.\r\n"
  (scm->dsv-string '(("The" "quick," "brown")
                     ("fox" "jumps" "over")
                     ("the" "lazy" "dog."))
                   #:format 'rfc4180))



(test-equal "dsv-string->scm: Unicode characters"
  '(("а1" "б1" "в1")
    ("а2" "б2" "в2"))
  (dsv-string->scm (string-join '("а1:б1:в1"
                                  "а2:б2:в2")
                                "\n")
                   #:format 'unix))

(test-equal "dsv-string->scm: Unicode characters: RFC4180"
  '(("Weiß" "White" "Белый")
    ("Grün" "Green" "Зелёный"))
  (dsv-string->scm (string-join '("\"Weiß\",\"White\",\"Белый\""
                                  "\"Grün\",\"Green\",\"Зелёный\"")
                                "\n")
                   #:format 'rfc4180))


;;; guess-delimiter

(test-equal "guess-delimiter: comma 1" #\,     (guess-delimiter "a,b,c"))
(test-equal "guess-delimiter: comma 2" #\,     (guess-delimiter "a,b,c,d:e"))
(test-equal "guess-delimiter: colon 1" #\:     (guess-delimiter "a:b:c"))
(test-equal "guess-delimiter: colon 2" #\:     (guess-delimiter "a,b:c:d:e"))
(test-equal "guess-delimiter: tab"     #\tab   (guess-delimiter "a	b	c"))
(test-equal "guess-delimiter: space"   #\space (guess-delimiter "a b c"))
(test-equal "guess-delimiter: #f"      #f      (guess-delimiter "a,b:c"))

(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "dsv")

(exit (zero? exit-status))

;;; dsv.scm ends here.

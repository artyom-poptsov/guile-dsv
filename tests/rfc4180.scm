;;; rfc4180.scm -- Tests for RFC 4180 parser.

;; Copyright (C) 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(test-begin "rfc4180")

(test-assert "dsv-string->list"
  (and (equal? '(("a" "b"))
               (dsv-string->scm "a,b" #\, #:format 'rfc4180))
       (equal? '(("a,b" "c"))
               (dsv-string->scm "\"a,b\",c" #\, #:format 'rfc4180))
       (equal? '(("a,b\nc" "d"))
               (dsv-string->scm "\"a,b\nc\",d" #\, #:format 'rfc4180))))

(test-assert "dsv-read"
  (and (equal? '(("a" "b" "c\nd,e" "f"))
               (call-with-input-string
                "a,b,\"c\nd,e\",f"
                (cut dsv->scm <> #\, #:format 'rfc4180)))
       (equal? '(("a\nb\nc\nd"))
               (call-with-input-string
                "\"a\nb\nc\nd\""
                (cut dsv->scm <> #\, #:format 'rfc4180)))
       (equal? '(("aaa" "b\"bb" "ccc"))
               (call-with-input-string
                "\"aaa\",\"b\"\"bb\",\"ccc\""
                (cut dsv->scm <> #:format 'rfc4180)))))

(test-assert "dsv-read, error handling"
  (and (catch 'dsv-parser-error
         (lambda ()
          (call-with-input-string
           "\"a"
           (cut dsv->scm <> #:format 'rfc4180))
          #f)
         (const #t))
       (catch 'dsv-parser-error
         (lambda ()
          (call-with-input-string
           "\"a\nb"
           (cut dsv->scm <> #:format 'rfc4180))
          #f)
         (const #t))))

(test-assert "scm->dsv-string"
  (and (let ((data '(("aaa" "b\"bb" "ccc"))))
         (equal? data
                 (dsv-string->scm
                  (scm->dsv-string data #:format 'rfc4180)
                  #\,
                  #:format 'rfc4180)))))


(test-end "rfc4180")

(exit (= (test-runner-fail-count (test-runner-current)) 0))

;;; rfc4180.scm ends here.

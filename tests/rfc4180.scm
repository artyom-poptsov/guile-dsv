;;; rfc4180.scm -- Tests for RFC 4180 parser.

;; Copyright (C) 2015-2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
             (ice-9 hash-table)
             (scheme documentation)
             (tests test)
             (dsv)
             ((dsv rfc4180) #:select (make-special-character-set))
             (dsv fsm dsv-context))


(define %test-name "rfc4180")

(test-begin %test-name)
(configure-test-logging! %test-name)

(define-with-docs %crlf
  "Carriage Return + Line Feed"
  "\r\n")


;; Writer.

(test-equal "rfc4180-writer-quote-field"
  (list #\" 'a #\")
  (rfc4180-writer-quote-field '(a)))

(test-equal "rfc4180-writer-should-be-enclosed?: #f"
  #f
  (rfc4180-writer-should-be-enclosed? (make-dsv-context
                                       '((a b c) (d e f))
                                       #:delimiter #\,
                                       #:line-break "\r\n"
                                       #:char-mapping (alist->hash-table '((#\" . #\")))
                                       #:custom-data (make-special-character-set #\,))
                                      "hello world"))

(test-equal "rfc4180-writer-should-be-enclosed?: #t"
  #t
  (rfc4180-writer-should-be-enclosed? (make-dsv-context
                                       '((a b c) (d e f))
                                       #:delimiter #\,
                                       #:line-break "\r\n"
                                       #:char-mapping (alist->hash-table '((#\" . #\")))
                                       #:custom-data (make-special-character-set #\,))
                                      "hello, \"world\""))

(test-assert "rfc4180-writer-process-field"
  (let ((context (make-dsv-context
                  '((a b c) (d e f))
                  #:delimiter #\,
                  #:line-break "\r\n"
                  #:char-mapping (alist->hash-table '((#\" . #\")))
                  #:custom-data (make-special-character-set #\,))))
    (rfc4180-writer-process-field context "hello, \"world\"")))


;;; dsv->scm

(test-equal "dsv->scm: input 1"
  '(("a" "b" "c\nd,e" "f"))
  (call-with-input-string
      "a,b,\"c\nd,e\",f"
    (cut dsv->scm <> #\, #:format 'rfc4180)))

(test-equal "dsv->scm: input 2"
  '(("a\nb\nc\nd"))
  (call-with-input-string
      "\"a\nb\nc\nd\""
    (cut dsv->scm <> #\, #:format 'rfc4180)))

(test-equal "dsv->scm: input 3"
  '(("aaa" "b\"bb" "ccc"))
  (call-with-input-string
      (string-join '("\"aaa\",\"b\"\"bb\",\"ccc\"")
                   %crlf)
    (cut dsv->scm <> #:format 'rfc4180)))

;; Check handling of quoted final fields in CRLF context
(test-equal "dsv->scm: CRLF 1"
  '(("aaa"))
  (call-with-input-string
      (string-join '("\"aaa\""
                     "")
                   %crlf)
    (cut dsv->scm <> #:format 'rfc4180)))

(test-equal "dsv->scm: CRLF 2"
  '(("aaa" "bbb")
    ("c\"cc" "ddd")
    ("" "e\""))
  (call-with-input-string
      (string-join '("aaa,\"bbb\""
                     "\"c\"\"cc\",ddd"
                     ",\"e\"\"\"")
                   %crlf)
    (cut dsv->scm <> #:format 'rfc4180)))

;; Check handling of empty quoted strings.
(test-equal "dsv->scm: empty quoted strings"
  '((""))
  (call-with-input-string
      "\"\""
    (cut dsv->scm <> #:format 'rfc4180)))

(test-equal "dsv->scm: A quoted field that contains CRLF"
  '(("a" "b\r\nc" "d"))
  (call-with-input-string
      (string-join '("a,\"b\r\nc\",d")
                   %crlf)
    (cut dsv->scm <> #:format 'rfc4180)))

(test-error "dsv->scm: EOF in a quoted field"
  (call-with-input-string
      "\"test"
    (cut dsv->scm <> #:format 'rfc4180)))

(test-error "dsv->scm: quote in an unquoted field"
  (call-with-input-string
      "a\",b"
    (cut dsv->scm <> #:format 'rfc4180)))


;; We allow LF line ending.
(test-equal "dsv->scm: LF line ending, unquoted"
  '(("aaa") ("bbb"))
  (call-with-input-string
      (string-join '("aaa"
                     "bbb")
                   "\n")
    (cut dsv->scm <> #:format 'rfc4180)))

(test-equal "dsv->scm: LF line ending, quoted"
  '(("aaa") ("bbb"))
  (call-with-input-string
      (string-join '("\"aaa\""
                     "\"bbb\"")
                   "\n")
    (cut dsv->scm <> #:format 'rfc4180)))


;;; Error testing

(test-error "dsv->scm, error handling: unquoted quote 1"
  'dsv-parser-error
  (call-with-input-string
      "\"a"
    (cut dsv->scm <> #:format 'rfc4180)))

(test-error "dsv->scm, error handling: unquoted quote 2"
  'dsv-parser-error
  (call-with-input-string
      "\"a\nb"
    (cut dsv->scm <> #:format 'rfc4180)))


;;; dsv-string

(test-equal "dsv-string->scm: two fields"
  '(("a" "b"))
  (dsv-string->scm "a,b" #\, #:format 'rfc4180))

(test-equal "dsv-string->scm: two fields, one is quoted with comma"
  '(("a,b" "c"))
  (dsv-string->scm "\"a,b\",c" #\, #:format 'rfc4180))

(test-equal "dsv-string->scm: two fields, one quoted with comma and newline"
  '(("a,b\nc" "d"))
  (dsv-string->scm "\"a,b\nc\",d" #\, #:format 'rfc4180))

(test-equal "dsv-string->scm: RFC 4180 with an empty field"
  '(("a" "b") ("" "d"))
  (dsv-string->scm "a,b\r\n,d\r\n" #\, #:format 'rfc4180))

(test-equal "dsv-string->scm: RFC 4180 with a trailing empty field"
  '(("a" "b") ("c" ""))
  (dsv-string->scm "a,b\r\nc,\r\n" #\, #:format 'rfc4180))



(test-equal "dsv-string->scm: Unicode characters"
  '(("а1" "б1" "в1")
    ("а2" "б2" "в2"))
  (dsv-string->scm (string-join '("а1,б1,в1"
                                  "а2,б2,в2")
                                "\r\n")
                   #:format 'rfc4180))


;;; scm->dsv

(test-equal "scm->dsv: one row, three fields, one with a double quote"
  "aaa,\"b\"\"bb\",ccc\r\n"
  (call-with-output-string
    (cut scm->dsv '(("aaa" "b\"bb" "ccc")) <>
         #:format 'rfc4180)))

(test-equal "scm->dsv: two rows, three fields in a row"
  "a,b,c\r\nd,e,f\r\n"
  (call-with-output-string
    (cut scm->dsv '(("a" "b" "c") ("d" "e" "f")) <>
         #:format 'rfc4180)))

(test-assert "scm->dsv-string"
  (and (let ((data '(("aaa" "b\"bb" "ccc"))))
         (equal? data
                 (dsv-string->scm
                  (scm->dsv-string data #:format 'rfc4180)
                  #\,
                  #:format 'rfc4180)))))


;;; guess-delimiter

(test-equal "guess-delimiter: comma 1" #\,     (guess-delimiter "a,b,c"))
(test-equal "guess-delimiter: comma 2" #\,     (guess-delimiter "a,b,c,d:e"))
(test-equal "guess-delimiter: colon 1" #\:     (guess-delimiter "a:b:c"))
(test-equal "guess-delimiter: colon 2" #\:     (guess-delimiter "a,b:c:d:e"))
(test-equal "guess-delimiter: tab"     #\tab   (guess-delimiter "a	b	c"))
(test-equal "guess-delimiter: space"   #\space (guess-delimiter "a b c"))
(test-equal "guess-delimiter: #f"      #f      (guess-delimiter "a,b:c"))

(test-equal "guess-delimiter, custom: dash, comma"
  #\-
  (guess-delimiter "a-b-c" '(#\- #\,)))

(test-equal "guess-delimiter, custom: dash"
  #f
  (guess-delimiter "a,b,c" '(#\-)))

(test-equal "guess-delimiter, custom: empty list"
  #f
  (guess-delimiter "a,b,c" '()))


(define exit-status (test-runner-fail-count (test-runner-current)))

(test-end "rfc4180")

(exit (zero? exit-status))

;;; rfc4180.scm ends here.

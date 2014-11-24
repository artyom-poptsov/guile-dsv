;;; dsv.scm -- DSV parser.

;; Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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


;;; Commentary:

;;
;; Parsers for delimiter separated values (DSV) format that widespread
;; in the Unix world.  Notable example of DSV is /etc/passwd file.
;; Default delimiter is set to a colon.
;;
;; Some examples:
;;
;;   (dsv-string->list "a:b:c")
;;   => '("a" "b" "c")
;;
;;   (dsv-string->list "a;b;c" #\;)
;;   => '("a" "b" "c")
;;
;;   (dsv-string->list "a,b\\,c" #\,)
;;   => '("a" "b,c")
;;
;;   (list->dsv-string '("a" "b" "c"))
;;   => "a:b:c"
;;
;;   (dsv-read (open-input-file "/etc/passwd"))
;;   => (...
;;       ("news" "x" "9" "13" "news" "/usr/lib/news" "/bin/false")
;;       ("root" "x" "0" "0" "root" "/root" "/bin/zsh"))
;;
;;   (guess-delimiter "a:b,c,d")
;;   => #\,
;;
;; These procedures are exported:
;; 
;;   dsv-string->list string [delimiter]
;;   list->dsv-string list [delimiter]
;;   dsv-read [port [delimiter]]
;;   dsv-write [port [delimiter]]
;;   guess-delimiter string
;;


;;; Code:

(define-module (dsv)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi  srfi-1)
  #:use-module (srfi  srfi-26)

  ;; escape-special-chars
  #:use-module (string transform)

  #:export (dsv-string->list
            list->dsv-string
            dsv-read
            dsv-write
            guess-delimiter))

;; Default delimiter for DSV
(define %default-delimiter #\:)

;; List of known delimiters
(define %known-delimiters '(#\, #\: #\tab #\space))


(define* (dsv-string->list string #:optional (delimiter %default-delimiter))
  "Convert DSV from a STRING a list using a DELIMITER.  If DELIMITER
is not set, use the default delimiter (colon).  Return newly created
list."
    (string-split/escaped string delimiter))


(define* (list->dsv-string list #:optional (delimiter %default-delimiter))
  "Convert a LIST to DSV string using DELIMITER.
If DELIMITER is not set, use the default delimiter (colon).  Return a
DSV string.

Example:

  (list->dsv-string '(\"a\" \"b\" \"c\"))
  => \"a:b:c\"
"
  (let* ((escaped-list (map (cut escape-special-chars <> delimiter #\\)
                            list)))
    (string-join escaped-list (string delimiter))))


(define* (dsv-read #:optional
                   (port      (current-input-port))
                   (delimiter %default-delimiter))
  "Read DSV from PORT.  If port is not set, read from default input
port.  If delimiter is not set, use the default
delimiter (colon). Return a list of values."
  (let parse ((dsv-list '())
              (line     (read-line port)))
    (if (not (eof-object? line))
        (parse (cons (string-split/escaped line delimiter) dsv-list)
               (read-line port))
        dsv-list)))


(define* (dsv-write list #:optional
                    (port      (current-output-port))
                    (delimiter %default-delimiter))
  "Write a LIST of values as DSV to a PORT.  If port is not set,
write to default output port.  If delimiter is not set, use the
default delimiter (colon)."
  (let ((dsv-record-list (map (cut list->dsv-string <> delimiter)
                              list)))
    (for-each (cut write-line <> port)
              dsv-record-list)))


(define (guess-delimiter string)
  "Guess a DSV STRING delimiter."
  (let* ((delimiter-list
          (map (lambda (d)
                 (cons d (length (string-split/escaped string d))))
               %known-delimiters))
         (guessed-delimiter-list
          (fold (lambda (a prev)
                  (if (not (null? prev))
                      (let ((a-count (cdr a))
                            (b-count (cdar prev)))
                        (cond ((> a-count b-count) (list a))
                              ((= a-count b-count) (append (list a) prev))
                              (else prev)))
                      (list a)))
                '()
                delimiter-list)))
    (and (= (length guessed-delimiter-list) 1)
         (caar guessed-delimiter-list))))


(define (string-split/escaped str delimiter)
  "Split a string STR into the list of the substrings delimited by appearances
of the DELIMITER.

This procedure is simlar to string-split, but works correctly with
escaped delimiter -- that is, skips it.  E.g.:

  (string-split/escaped \"car:cdr:ca\\:dr\" #\\:)
  => (\"car\" \"cdr\" \"ca\\:dr\")
"
  (let ((fields (string-split str delimiter)))
    (fold (lambda (field prev)
            (if (and (not (null? prev))
                     (string-suffix? "\\" (last prev)))
                (append (drop-right prev 1)
                        (list (string-append 
                               (string-drop-right (last prev) 1)
                               (string delimiter)
                               field)))
                (append prev (list field))))
          '()
          fields)))

;;; dsv.scm ends here.

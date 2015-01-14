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
;; Procedures for working with delimiter-separated values (DSV) format that is
;; widespread in the Unix world.  Notable examples of DSV are /etc/passwd and
;; /etc/inittab files.  Default delimiter is set to a colon.
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

  ;; DSV
  #:use-module (dsv rfc4180)
  #:use-module (dsv unix)

  #:export (dsv-string->list
            list->dsv-string
            dsv-read
            dsv-write
            guess-delimiter))

;; Default delimiter for DSV
(define %default-delimiter #\:)

;; List of known delimiters
(define %known-delimiters '(#\, #\: #\; #\| #\tab #\space))

(define* (dsv-string->list str
                           #:optional (delimiter %default-delimiter)
                           #:key (format 'unix))
  "Convert a DSV string STR to a list of values using a DELIMITER.  If the
DELIMITER is not set, use the default delimiter (colon).  Return a list of
values."
  (case format
    ((unix)
     (dsv-string->list/unix str delimiter))
    ((rfc4180)
     (dsv-string->list/rfc4180 str delimiter))
    (else
     (error "Unknown format" format))))

(define* (list->dsv-string lst
                           #:optional (delimiter %default-delimiter)
                           #:key (format 'unix))
  "Convert a list LST to a DSV string using a DELIMITER.  If the DELIMITER is
not set, use the default delimiter (colon).  Return a DSV string.

Example:

  (list->dsv-string '(\"a\" \"b\" \"c\"))
  => \"a:b:c\"
"
  (case format
    ((unix)
     (list->dsv-string/unix lst delimiter))
    ((rfc4180)
     (list->dsv-string/rfc4180 lst delimiter))
    (else
     (error "Unknown format" format))))


(define* (dsv-read #:optional
                   (port      (current-input-port))
                   (delimiter %default-delimiter)
                   #:key
                   (format         'unix)
                   (comment-symbol #\#))
  "Read DSV data from a PORT.  If the PORT is not set, read from the default
input port.  If a DELIMITER is not set, use the default delimiter (colon).
Skip lines commented with a COMMENT-SYMBOL.  Return a list of values."

  (define (commented? line)
    "Check if the LINE is commented."
    (string-prefix? (string comment-symbol) (string-trim line)))

  (case format
    ((unix)
     (let parse ((dsv-list '())
                 (line     (read-line port)))
       (if (not (eof-object? line))
           (if (not (commented? line))
               (parse (cons (dsv-string->list/unix line delimiter) dsv-list)
                      (read-line port))
               (parse dsv-list (read-line port)))
           (reverse dsv-list))))
    ((rfc4180)
     (let parse ((dsv-list '())
                 (line     (read-line port)))
       (if (not (eof-object? line))
           (parse (dsv-string->list/rfc4180 line delimiter) (read-line port))
           (reverse dsv-list))))
    (else
     (error "Unknown format" format))))

(define* (dsv-write lst
                    #:optional
                    (port      (current-output-port))
                    (delimiter %default-delimiter)
                    #:key
                    (format    'unix))
  "Write a list of values LST as a sequence of DSV strings to a PORT.
If the PORT is not set, write to the default output port.  If a DELIMITER is
not set, use the default delimiter (colon).  FORMAT allows to specify a DSV
format style."
  (let ((dsv-record-list (map (cut list->dsv-string <> delimiter #:format format)
                              lst)))
    (for-each (cut write-line <> port)
              dsv-record-list)))


(define (guess-delimiter str)
  "Guess a DSV string STR delimiter."
  (let* ((delimiter-list
          (map (lambda (d)
                 (cons d (length (string-split/escaped str d))))
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

;;; dsv.scm ends here.

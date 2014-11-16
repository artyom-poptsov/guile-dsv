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
;;   (dsv-string-split "car:cdr:ca\\:dr" #\:)
;;   => ("car" "cdr" "ca\\:dr")
;;
;;   (list->dsv-string '("a" "b" "c"))
;;   => "a:b:c"
;;
;;   (dsv-read (open-input-file "/etc/passwd"))
;;   => (...
;;       ("news" "x" "9" "13" "news" "/usr/lib/news" "/bin/false")
;;       ("root" "x" "0" "0" "root" "/root" "/bin/zsh"))
;;
;; These procedures are exported:
;; 
;;   dsv-string->list string [delimiter]
;;   list->dsv-string list [delimiter]
;;   dsv-read [port [delimiter]]
;;   dsv-write [port [delimiter]]
;;   dsv-string-split string [delimiter]
;;


;;; Code:

(define-module (dsv)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi  srfi-1)

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


(define (dsv-string->list string . delimiter)
  "Convert DSV from a STRING a list using a DELIMITER.  If DELIMITER
is not set, use the default delimiter (colon).  Return newly created
list."
  (let ((delimiter (if (not (null? delimiter))
                       (car delimiter)
                       %default-delimiter)))
    (dsv-string-split string delimiter)))



(define (list->dsv-string list . delimiter)
  "Convert a LIST to DSV string using DELIMITER.
If DELIMITER is not set, use the default delimiter (colon).  Return a
DSV string.

Example:

  (list->dsv-string '(\"a\" \"b\" \"c\"))
  => \"a:b:c\"
"
  (let ((delimiter (if (not (null? delimiter))
                       (car delimiter)
                       %default-delimiter)))
    (let append-field ((rec list))
      (string-append
       (escape-special-chars (car rec) delimiter #\\)
       (if (not (null? (cdr rec)))
           (string-append
            (string delimiter)
            (append-field (cdr rec)))
           "")))))


(define (dsv-read . args)
  "Read DSV from PORT.  If port is not set, read from default input
port.  If delimiter is not set, use the default
delimiter (colon). Return a list of values."
  (let ((port      (if (not (null? args))
                       (car args)
                       (current-input-port)))
        (delimiter (if (and (not (null? args))
                            (not (null? (cdr args))))
                       (cadr args)
                       %default-delimiter)))
    (let parse ((dsv-list '()))
      (let ((line (read-line port)))
        (if (not (eof-object? line))
            (parse (cons (dsv-string-split line delimiter) dsv-list))
            (begin
              (set! dsv-list
                    (map
                     (lambda (dsv-data)
                       (map (lambda (field)
                              (regexp-substitute/global
                               #f "\\\\:" field 'pre ":" 'post))
                            dsv-data))
                     dsv-list))
              (reverse dsv-list)))))))


(define (dsv-write list . args)
  "Write a LIST of values as DSV to a PORT.  If port is not set,
write to default output port.  If delimiter is not set, use the
default delimiter (colon)."
  (let ((port      (if (not (null? args))
                       (car args)
                       (current-output-port)))
        (delimiter (if (and (not (null? args))
                            (not (null? (cdr args))))
                       (cadr args)
                       %default-delimiter)))

    (let ((dsv (map (lambda (data)
                      (or (null? data)
                          (list->dsv-string data delimiter)))
                    list)))
      (for-each
       (lambda (dsv-record)
         (write-line dsv-record port))
       dsv))))


(define (guess-delimiter string)
  "Guess a DSV STRING delimiter."
  (let* ((delimiter-list
          (map (lambda (d)
                 (cons d (length (dsv-string-split string d))))
               %known-delimiters))
         (guessed-delimiter
          (fold (lambda (a b)
                  (let ((a-count (cdr a))
                        (b-count (cdr b)))
                    (if (> a-count b-count)
                        a
                        b)))
                (car delimiter-list)
                delimiter-list)))
    (and (> (cdr guessed-delimiter) 1)
         (car guessed-delimiter))))


;; TODO: Probably the procedure should be rewritten or replaced with
;;       some standard procedure.
(define (dsv-string-split string . delimiter)
  "Split the STRING into the list of the substrings delimited by
appearances of the DELIMITER.  If DELIMITER is not set, use the
default delimiter (colon).

This procedure is simlar to string-split, but works correctly with
escaped delimiter -- that is, skips it.  E.g.:

  (dsv-string-split \"car:cdr:ca\\:dr\" #\\:)
  => (\"car\" \"cdr\" \"ca\\:dr\")
"
  (let* ((delimiter (if (not (null? delimiter))
                        (car delimiter)
                        %default-delimiter))
         (delimiter? (lambda (idx)
                      (eq? (string-ref string idx) delimiter)))
         (dsv-list  '())
         (len       (string-length string))
         (start     0))

    (do ((i 0 (1+ i)))
        ((= i len))
      (cond
       ((and (delimiter? i)
             (= i 0))
        (set! dsv-list (append dsv-list (list "")))
        (set! start 1))
       ((= i (1- len))
        (if (delimiter? i)
            (set! dsv-list
                  (append dsv-list (list (substring string start i)
                                         "")))
            (set! dsv-list
                  (append dsv-list (list (substring string start (1+ i)))))))
       ((and (delimiter? i)
             (not (eq? (string-ref string (1- i)) #\\)))
        (set! dsv-list
              (append dsv-list (list (substring string start i))))
        (set! start (1+ i)))))

    dsv-list))

;;; dsv.scm ends here.

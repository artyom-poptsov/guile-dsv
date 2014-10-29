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

(define-module (fresco dsv-parser)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)

  ;; escape-special-chars
  #:use-module (string transform)

  #:export (dsv-string->list
            list->dsv-string
            dsv-read
            dsv-write
            dsv-string-split))

;; Default delimiter for DSV
(define %default-delimiter #\:)


(define (dsv-string->list string . delimiter)
  "Convert DSV from a string @var{string} to a list usigng a
@var{delimiter}.  If @var{delimiter} is not set, use the default
delimiter (colon).  Return newly created list.

Examples:

@lisp
  (dsv-string->list \"a:b:c\")
  => '(\"a\" \"b\" \"c\")

  (dsv-string-split \"car:cdr:ca\\:dr\" #\:)
  => (\"car\" \"cdr\" \"ca\\:dr\")
@end lisp
"
  (let ((delimiter (if (not (null? delim))
                       (car delim)
                       %default-delimiter)))
    (dsv-string-split string delimiter)))



(define (list->dsv-string list . delimiter)
  "Convert a @var{list} to DSV string using @var{delimiter}.
If @var{delimitter} is not set, use the default delimiter (colon).
Return a DSV string.

Example:

@lisp
  (list->dsv-string '(\"a\" \"b\" \"c\"))
  => \"a:b:c\"
@end lisp
"
  (let ((delimiter (if (not (null? delim))
                     (car delim)
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
  "Read DSV from @var{port}.  If port is not set, read from default
input port.  If delimiter is not set, use the default
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
  "Write a @var{list} of values as DSV to a port.  If port is not set,
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
                      (if (not (null? data))
                          (list->dsv-string data delimiter)))
                    list)))
      (for-each
       (lambda (dsv-record)
         (begin
           (display dsv-record port)
           (newline port)))
       dsv))))


;; TODO: Probably the procedure should be rewritten or replaced with
;;       some standard procedure.
(define (dsv-string-split string . delimiter)
  "Split the @var{string} into the list of the substrings delimited by
appearances of the @var{delimiter}.  If @var{delimiter} is not set,
use the default delimiter (colon).

This procedure is simlar to string-split, but works correctly with
escaped delimiter -- that is, skips it. E.g.:

@lisp
  (dsv-string-split \"car:cdr:ca\\:dr\" #\:)
  => (\"car\" \"cdr\" \"ca\\:dr\")
@end lisp
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
       ((and (= i (1- len))
             (not (delimiter? i)))
        (set! dsv-list
              (append dsv-list (list (substring string start (1+ i))))))
       ((and (delimiter? i)
             (not (eq? (string-ref string (1- i)) #\\)))
        (set! dsv-list
              (append dsv-list (list (substring string start i))))
        (set! start (1+ i)))))

    dsv-list))

;;; dsv.scm ends here.

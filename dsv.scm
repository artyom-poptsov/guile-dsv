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

  #:export (dsv-string->list
            list->dsv-string
            dsv-read
            dsv-write
            guess-delimiter))

;; Default delimiter for DSV
(define %default-delimiter #\:)

;; List of known delimiters
(define %known-delimiters '(#\, #\: #\; #\| #\tab #\space))

(define (debug fmt . args)
  (let ((fmt (string-append "DEBUG: " fmt)))
    (apply format #t fmt args)))


(define (unescape-special-char str special-char escape-char)
  (regexp-substitute/global #f (string escape-char special-char) str
                            'pre (string special-char) 'post))

(define (dsv-string->list/rfc4180 str delimiter)

  (define (string-drop-both s n)
    "Drop N chars from a string S on the both left and right sides."
    (string-drop-right (string-drop s n) n))

  (define (quotation-status field)
    "Get quotation status for a FIELD."
    (cond
     ((regexp-match? (string-match "^\"[^\"].*[^\"]\"$" field))
      'quoted)
     ((regexp-match? (string-match "^\"[^\"].*" field))
      'quote-begin)
     ((regexp-match? (string-match ".*[^\"]\"$" field))
      'quote-end)))

  (define all-double-quotes-escaped?
    (case-lambda
      "Check if all the double-quotes are escaped."
      ((field)
       (even? (string-count field #\")))
      ((field skip)
       (even? (string-count (string-drop field skip) #\")))
      ((field skip skip-right)
       (even? (string-count (string-drop-right (string-drop field skip)
                                               skip-right)
                            #\")))))

  (let fold-fields ((fields (string-split str delimiter))
                    (prev   '())
                    (state  'add))
    (debug "state: ~a~%" state)
    (if (not (null? fields))
        (let ((field (car fields)))
          (debug "field: ~a~%" field)
          (case state
            ((add)
             (cond
              ;; Handle a properly double-quoted field, such as:
              ;;   "\"Hello World!\""
              ((equal? (quotation-status field) 'quoted)
               (if (all-double-quotes-escaped? field)
                   (fold-fields (cdr fields)
                                (cons (string-drop-both field 1) prev)
                                'add)
                   (error "A field contains unescaped double-quotes" field)))
              ;; Handle the beginning of a double-quoted field:
              ;;   "\"Hello"
              ((equal? (quotation-status field) 'quote-begin)
               (if (all-double-quotes-escaped? field 1)
                   (fold-fields (cdr fields)
                                (cons (string-drop field 1) prev)
                                'append)
                   (error "A field contains unescaped double-quotes" field)))
              ;; Handle an unquoted field with double-quotes inside it:
              ;;   "Hello World\""
              ((string-index field #\")
               (error "Unexpected double-quote inside of an unquoted field"
                      field))
              ;; Handle line breaks inside of an unquoted field:
              ;;   "Hello\r\nWorld!"
              ((string-contains field "\r\n")
               (error "Unexpected line break (CRLF) inside of an unquoted field"
                      field))
              ;; Handle unquoted fields:
              ;;   "Hello World!"
              (else
               (let ((unescaped-field (unescape-special-char field #\" #\")))
                 (fold-fields (cdr fields) (cons unescaped-field prev) 'add)))))

            ((append)
             (debug "append: quotation-status: ~a~%" (quotation-status field))
             (cond
              ((or (equal? (quotation-status field) 'quote-end)
                   (equal? (quotation-status field) 'quoted))
               (if (all-double-quotes-escaped? field 0 1)
                   (let* ((prev-field (car prev))
                          (field (string-append prev-field
                                                (string delimiter)
                                                (string-drop-right field 1)))
                          (field (unescape-special-char field #\" #\")))
                     (fold-fields (cdr fields) (cons field (drop prev 1))
                                  'add))
                   (error "A field contains unescaped double-quotes" field)))
              (else
               (if (all-double-quotes-escaped? field)
                   (let* ((prev-field (car prev))
                          (field      (string-append prev-field
                                                     (string delimiter)
                                                     field))
                          (field      (unescape-special-char field #\" #\")))
                     (fold-fields (cdr fields) (cons field (drop prev 1))
                                  'append))
                   (error "A field contains unescaped double-quotes" field)))))))
        (reverse prev))))

(define (dsv-string->list/unix str delimiter)
    (string-split/escaped str delimiter))

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


(define (list->dsv-string/rfc4180 lst delimiter)

  (define (should-be-enclosed? field)
    "Check if a FIELD should be enclosed in double-quotes."
    (or (string-index    field (char-set delimiter #\" #\newline))
        (string-contains field (string #\cr #\newline))))

  (define (escape-double-quotes field)
    "Escape each double-quote in a FIELD with additional double-quote."
    (escape-special-chars field #\" #\"))

  (define (quote-field field)
    "Quote a FIELD with double-quotes."
    (string-append (string #\") field (string #\")))

  (let ((quoted-lst (map (lambda (field)
                           (let ((escaped-field (escape-double-quotes field)))
                             (if (should-be-enclosed? escaped-field)
                                 (quote-field escaped-field)
                                 field)))
                         lst)))
    (string-append (string-join quoted-lst (string delimiter)) (string #\cr))))

(define (list->dsv-string/unix lst delimiter)
  (let ((escaped-list (map (cut escape-special-chars <> delimiter #\\)
                           lst)))
    (string-join escaped-list (string delimiter))))

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
                   (comment-symbol #\#))
  "Read DSV data from a PORT.  If the PORT is not set, read from the default
input port.  If a DELIMITER is not set, use the default delimiter (colon).
Skip lines commented with a COMMENT-SYMBOL.  Return a list of values."

  (define (commented? line)
    "Check if the LINE is commented."
    (string-prefix? (string comment-symbol) (string-trim line)))

  (let parse ((dsv-list '())
              (line     (read-line port)))
    (if (not (eof-object? line))
        (if (not (commented? line))
            (parse (cons (string-split/escaped line delimiter) dsv-list)
                   (read-line port))
            (parse dsv-list (read-line port)))
        (reverse dsv-list))))


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

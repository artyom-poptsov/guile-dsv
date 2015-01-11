;;; rfc4180.scm -- DSV parser for RFC 4180 format.

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


;;; Commentary:


;;; Code:

(define-module (dsv rfc4180)
  #:use-module (ice-9 regex)
  #:export (dsv-string->list/rfc4180
            list->dsv-string/rfc4180))

(define (debug fmt . args)
  (let ((fmt (string-append "DEBUG: " fmt)))
    (apply format #t fmt args)))


(define (unescape-special-char str special-char escape-char)
  (regexp-substitute/global #f (string escape-char special-char) str
                            'pre (string special-char) 'post))

(define* (string-drop-both s n #:optional (n-right n))
  "Drop N chars from a string S on the both left and right sides."
  (string-drop-right (string-drop s n) n-right))

(define-syntax case-pred
  (syntax-rules (else)
    ((_ pred key ((datum ...) exp) ...)
     (cond
      ((or (pred key datum) ...) exp) ...))
    ((_ pred key ((datum ...) exp) ... (else else-exp))
     (cond
      ((or (pred key datum) ...) exp) ...
      (else else-exp)))))

(define (dsv-string->list/rfc4180 str delimiter)

  (define (quotation-status field)
    "Get quotation status for a FIELD."
    (case-pred (lambda (field regexp) (regexp-match? (string-match regexp field)))
               field
     (("^\".*\"$") 'quoted)
     (("^\".+")    'quote-begin)
     ((".+\"$")    'quote-end)
     (("^\"$")     'quote-begin-or-end)))

  (define* (all-double-quotes-escaped? field #:optional (skip 0) (skip-right 0))
    "Check if all the double-quotes are escaped."
    (even? (string-count (string-drop-both field skip skip-right) #\")))

  (let fold-fields ((fields (string-split str delimiter))
                    (prev   '())
                    (state  'add))
    (debug "state: ~a~%" state)
    (if (not (null? fields))
        (let ((field (car fields)))
          (debug "field: ~a~%" field)
          (case state
            ((add)
             (case (quotation-status field)
              ;; Handle a properly double-quoted field, such as:
              ;;   "\"Hello World!\""
              ((quoted)
               (if (all-double-quotes-escaped? field)
                   (let ((unquoted-field (string-drop-both field 1)))
                     (fold-fields (cdr fields)
                                  (cons (unescape-special-char unquoted-field
                                                               #\" #\")
                                        prev)
                                  'add))
                   (error "A field contains unescaped double-quotes" field)))
              ;; Handle the beginning of a double-quoted field:
              ;;   "\"Hello"
              ((quote-begin)
               (if (all-double-quotes-escaped? field 1)
                   (fold-fields (cdr fields) (cons (string-drop field 1) prev)
                                'append)
                   (error "A field contains unescaped double-quotes" field)))
              ((quote-begin-or-end)
               (fold-fields (cdr fields) (cons "" prev) 'append))
              (else
               (cond
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
                   (fold-fields (cdr fields) (cons unescaped-field prev)
                                'add)))))))

            ((append)
             (debug "append: quotation-status: ~a~%" (quotation-status field))
             (case (quotation-status field)
              ((quote-end quoted)
               (if (all-double-quotes-escaped? field 0 1)
                   (let* ((prev-field (car prev))
                          (field (string-append prev-field
                                                (string delimiter)
                                                (string-drop-right field 1)))
                          (field (unescape-special-char field #\" #\")))
                     (fold-fields (cdr fields) (cons field (drop prev 1))
                                  'add))
                   (error "A field contains unescaped double-quotes" field)))
              ((quote-begin-or-end)
               (let* ((prev-field (car prev))
                      (field (string-append prev-field (string delimiter) "")))
                 (fold-fields (cdr fields) (cons field (drop prev 1))
                              'add)))
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

;;; rfc4180.scm ends here

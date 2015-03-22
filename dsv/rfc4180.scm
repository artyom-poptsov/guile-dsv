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
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (ice-9 rdelim)
  #:export (dsv-string->list/rfc4180
            list->dsv-string/rfc4180
            dsv-read/rfc4180))

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

  (define (get-quotation-status field)
    "Get quotation status for a FIELD."
    (case-pred (lambda (field regexp) (regexp-match? (string-match regexp field)))
               field
     (("^\".*\"$") 'quoted)
     (("^\".+")    'quote-begin)
     ((".+\"$")    'quote-end)
     (("^\"$")     'quote-begin-or-end)))

  (define* (all-double-quotes-escaped? field)
    "Check if all the double-quotes are escaped."
    (even? (string-count field #\")))

  (let fold-fields ((record (string-split str delimiter))
                    (buffer '())
                    (prev   '())
                    (state  'read))
    (debug "state: ~a~%" state)
    (case state

      ((read)
       (let ((field (or (null? record) (car record))))
         (debug "field: ~a; buffer: ~a~%" field buffer)
         (cond
          ((null? record)
           (fold-fields record buffer prev 'end))
          ((null? buffer)
           (case (get-quotation-status field)
             ((quote-begin quote-begin-or-end)
              (fold-fields (cdr record) (cons field buffer) prev 'read))
             (else
              (fold-fields (cdr record) (cons field buffer) prev 'join))))
          (else
           (case (get-quotation-status field)
             ((quote-end quote-begin-or-end)
              (fold-fields (cdr record) (cons field buffer) prev 'join))
             (else
              (fold-fields (cdr record) (cons field buffer) prev 'read)))))))

      ((join)
       (fold-fields record (string-join (reverse buffer) (string delimiter))
                    prev 'validate))

      ((validate)
       (case (get-quotation-status buffer)
         ((quoted)
          (cond
           ((not (all-double-quotes-escaped? buffer))
            (error "A field contains unescaped double-quotes" buffer))))
         (else
          (cond
           ((string-index buffer #\")
            (error "A field contains unescaped double-quotes" buffer))
           ((string-contains buffer "\r\n")
            (error "Unexpected line break (CRLF) inside of an unquoted field"
                   buffer)))))
       (fold-fields record buffer prev 'add))

      ((add)
       (let* ((buffer (if (eq? (get-quotation-status buffer) 'quoted)
                          (string-drop-both buffer 1)
                          buffer))
              (buffer (unescape-special-char buffer #\" #\")))
         (fold-fields record '() (cons buffer prev) 'read)))

      ((end)
       (reverse prev)))))


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


;; XXX: COMMENT-SYMBOL is not used.
(define* (dsv-read/rfc4180 port delimiter comment-symbol)
  (let parse ((dsv-list '())
              (line     (read-line port)))
       (if (not (eof-object? line))
           (parse (dsv-string->list/rfc4180 line delimiter) (read-line port))
           (reverse dsv-list))))

;;; rfc4180.scm ends here

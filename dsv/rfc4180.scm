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

;; A parser of RFC 4180 <https://tools.ietf.org/html/rfc4180> data format
;; (Comma-Separated Values, CSV).


;;; Code:

(define-module (dsv rfc4180)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (ice-9 rdelim)
  #:use-module (scheme documentation)
  #:use-module (dsv common)
  #:use-module (dsv parser)
  #:export (make-parser
            make-string-parser
            scm->dsv
            scm->dsv-string
            dsv->scm
            dsv-string->scm
;            guess-delimiter
            ;; Variables
            %default-delimiter))


;;; Global variables

(define-with-docs %default-line-break
  "Default line break style as described in the RFC."
  "\r\n")

(define-with-docs %default-delimiter
  "Default field delimiter."
  #\,)


;;; Helper procedures

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
    ((_ pred key ((datum ...) exp) ... (else else-exp ...))
     (cond
      ((or (pred key datum) ...) exp) ...
      (else else-exp ...)))))

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

(define (validate-field state field)
  "Validate a FIELD."
  (case (get-quotation-status field)
    ((quoted)
     (cond
      ((not (all-double-quotes-escaped? field))
       (dsv-error state "A field contains unescaped double-quotes" field))))
    (else
     (cond
      ((string-index field #\")
       (dsv-error state "A field contains unescaped double-quotes" field))
      ((string-contains field "\r\n")
       (dsv-error state
                  "Unexpected line break (CRLF) inside of an unquoted field"
                  field))))))


;;; Writing


(define (escape-double-quotes field)
  "Escape each double-quote in a FIELD with additional double-quote."
  (escape-special-chars field #\" #\"))

(define (quote-field field)
  "Quote a FIELD with double-quotes."
  (string-append (string #\") field (string #\")))

(define* (scm->dsv scm port delimiter #:key (line-break %default-line-break))
  "Create a DSV document from a native SCM list.  Separate fields using a
DELIMITER and print the document to a specified PORT.  Optionally accept
LINE-BREAK argument which specifies the style of line breaks; default value is
CRLF."

  (define (should-be-enclosed? field)
    "Check if a FIELD should be enclosed in double-quotes."
    (or (string-index    field (char-set delimiter #\" #\newline))
        (string-contains field line-break)))

  (for-each
   (lambda (row)
     (display
      (string-join
       (map (lambda (field)
              (let ((escaped-field (escape-double-quotes field)))
                (if (should-be-enclosed? escaped-field)
                    (quote-field escaped-field)
                    field)))
            row)
       (string delimiter))
      port)
     (display line-break port))
   scm))

(define* (scm->dsv-string scm delimiter #:key (line-break %default-line-break))
  "Create a DSV string from a native SCM list.  Separate fields using a
DELIMITER.  Optionally accept LINE-BREAK argument which specifies the style of
line breaks; default value is CRLF.  Return a DSV string."
  (call-with-output-string
   (cut scm->dsv scm <> delimiter #:line-break line-break)))


;;; Reading

(define (make-parser port delimiter known-delimiters comment-prefix)
  (%make-parser port
                'rfc4180
                (if (eq? delimiter 'default)
                    %default-delimiter
                    delimiter)
                (if (eq? known-delimiters 'default)
                    %known-delimiters
                    known-delimiters)
                comment-prefix))

(define (make-string-parser str delimiter known-delimiters comment-prefix)
  (call-with-input-string str (cut make-parser <> delimiter known-delimiters
                                   comment-prefix)))


;; XXX: COMMENT-SYMBOL is not used.
;;
;; State machine:
;;
;;                 ,----------------------------------------------------------.
;;                 |            ,---.                                         |
;;                 v            V   |                                         |
;;  START ----->[read-ln]--->[read]-+->[join]----->[validate]--->[add-field]--'
;;                 |  A         |   |
;;                 |  |         |   '->[add-record]-.
;;                 |  |         |                   |
;;                 |  '---------'<------------------'
;;                 `--------------------------------------->[end]--------> STOP
;;
(define (dsv->scm parser)
  "Read DSV data from a PORT using a DELIMITER.  Return a native SCM list.
Throw a 'dsv-parser-error' on an error."
  (define* (fold-file #:key
                      (dsv-list     '())
                      (buffer       '())
                      (field-buffer '())
                      (record       '())
                      (field        #f)
                      (line         #f)
                      (state        'read-ln))

    (debug-fsm-transition state)

    (case state

      ;;   [read-ln]-+->[read]
      ;;             |
      ;;             '->[end]
      ((read-ln)
       (debug-fsm state "dsv-list: ~s~%" dsv-list)
       (debug-fsm state "buffer:   ~s~%" buffer)
       (let ((line (parser-read-line parser)))
         (debug-fsm state "line: ~s~%" line)
         (cond
          ((not (eof-object? line))
           (debug-fsm-transition state 'read)
           (fold-file #:dsv-list     dsv-list
                      #:buffer       buffer
                      #:field-buffer field-buffer
                      #:record       (parser-string-split parser line)
                      #:line         line
                      #:state        'read))
          ((and (eof-object? line) (null? buffer) (null? field-buffer))
           (debug-fsm-transition state 'end)
           (fold-file #:dsv-list     dsv-list
                      #:buffer       buffer
                      #:field-buffer field-buffer
                      #:record       record
                      #:line         line
                      #:state        'end))
          (else
           (dsv-error state "Premature end of file" (parser-port parser))))))

      ((read)
       (let ((field (or (null? record) (car record))))
         (debug-fsm state "field: ~s; field-buffer: ~s~%" field field-buffer)
         (cond
          ((null? record)
           (cond
            ((null? field-buffer)
             (debug-fsm-transition state 'add-record)
             (fold-file #:dsv-list     dsv-list
                        #:buffer       buffer
                        #:field-buffer field-buffer
                        #:record       record
                        #:line         line
                        #:state        'add-record))
            ;; A field contains '\n'.
            ;;   [read]--->[read-ln]
            (else
             (debug-fsm-transition state 'read-ln)
             (fold-file #:dsv-list     dsv-list
                        #:buffer       buffer
                        ;; XXX: Does it handles all the cases?
                        #:field-buffer (cons "\n" field-buffer)
                        #:record       record
                        #:line         line
                        #:state        'read-ln))))

          ;;      ,---.
          ;;      V   |
          ;;   [read]-+->[join]
          ((null? field-buffer)
           (fold-file #:dsv-list     dsv-list
                      #:buffer       buffer
                      #:field-buffer (list field)
                      #:record       (cdr record)
                      #:line         line
                      #:state        (case (get-quotation-status field)
                                       ((quote-begin quote-begin-or-end)
                                        (debug-fsm-transition state 'read)
                                        'read)
                                       (else
                                        (debug-fsm-transition state 'join)
                                        'join))))
          (else
           (fold-file #:dsv-list     dsv-list
                      #:buffer       buffer
                      #:field-buffer (cons field field-buffer)
                      #:record       (cdr record)
                      #:line         line
                      #:state        (case (get-quotation-status field)
                                       ((quote-end quote-begin-or-end)
                                        (debug-fsm-transition state 'join)
                                        'join)
                                       (else
                                        (debug-fsm-transition state 'read)
                                        'read)))))))

      ;;   [join]--->[validate]
      ((join)
       (debug-fsm-transition state 'validate)
       (debug-fsm state "field-buffer: ~s~%" field-buffer)
       (let* ((delimiter  (parser-delimiter->string parser))
              ;; XXX: Looks too hacky.  Should be rewritten in a more
              ;; elegant way.
              (join-field (lambda (field-elements)
                            (fold (lambda (elem prev)
                                    (cond
                                     ((not prev)
                                      elem)
                                     ((or (string=? "\n" elem)
                                          (string-suffix? "\n" prev))
                                      (string-append prev elem))
                                     (else
                                      (string-append prev delimiter elem))))
                                  #f
                                  (reverse field-elements))))
              (drop-cr    (lambda (s)
                            (if (string-suffix? "\r" s)
                                (string-drop-right s 1)
                                s))))
         (fold-file #:dsv-list     dsv-list
                    #:buffer       buffer
                    #:field-buffer (drop-cr (join-field field-buffer))
                    #:record       record
                    #:line         line
                    #:state        'validate)))

      ;;   [validate]-+->[add-field]
      ;;              |
      ;;              '-> ERROR
      ((validate)
       (validate-field state field-buffer)
       (debug-fsm-transition state 'add-field)
       (fold-file #:dsv-list     dsv-list
                  #:buffer       buffer
                  #:field-buffer field-buffer
                  #:record       record
                  #:line         line
                  #:state        'add-field))

      ;;   [add-field]--->[read]
      ((add-field)
       (debug-fsm-transition state 'read)
       (let ((field (if (eq? (get-quotation-status field-buffer) 'quoted)
                        (string-drop-both (unescape-special-char
                                           field-buffer #\" #\")
                                          1)
                        field-buffer)))
         (fold-file #:dsv-list     dsv-list
                    #:buffer       (cons field buffer)
                    #:field-buffer '()
                    #:record       record
                    #:line         line
                    #:state        'read)))

      ;;   [add-record]--->[read-ln]
      ((add-record)
       (debug-fsm-transition state 'read-ln)
       (fold-file #:dsv-list     (cons buffer dsv-list)
                  #:buffer       '()
                  #:field-buffer field-buffer
                  #:record       record
                  #:line         line
                  #:state        'read-ln))

      ;;   [end]---> STOP
      ((end)
       (debug-fsm-transition state 'STOP 'final)
       (reverse (map reverse dsv-list)))))

  (fold-file))

;; TODO: Fix it
(define guess-delimiter (make-delimiter-guesser dsv->scm))

;;; rfc4180.scm ends here

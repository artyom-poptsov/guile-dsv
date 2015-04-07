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
  #:use-module (srfi srfi-1)
  #:use-module ((string transform)
                #:select (escape-special-chars))
  #:use-module (ice-9 rdelim)
  #:export (dsv-string->list/rfc4180
            list->dsv-string/rfc4180
            dsv-read/rfc4180
            ;; Debugging
            set-debug!))


(define *debug?* #f)                    ;Does debug mode enabled?


(define (set-debug! enabled?)
  "Set debug mode to an ENABLED? value."
  (set! *debug?* enabled?))

(define (debug fmt . args)
  (and *debug?*
       (let ((fmt (string-append "DEBUG: " fmt)))
         (apply format #t fmt args))))

(define (debug-fsm state fmt . args)
  "Format and print a debug message from a finite-state machine (FSM)."
  (apply debug (format #f "[~a]: ~a" state fmt) args))

(define debug-fsm-transition
  (case-lambda
    "Debug a finite-state machine (FSM) transition."
    ((to)
     (debug "--->[~a]~%" to))
    ((from to)
     (debug "[~a]--->[~a]~%" from to))
    ((from to type)
     (case type
       ((final)
        (debug "[~a]---> ~a~%" from to))
       (else
        (debug "[~a]--->[~a]~%" from to))))))

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
       (debug-fsm-transition state 'ERROR 'final)
       (error "A field contains unescaped double-quotes" field))))
    (else
     (cond
      ((string-index field #\")
       (debug-fsm-transition state 'ERROR 'final)
       (error "A field contains unescaped double-quotes" field))
      ((string-contains field "\r\n")
       (debug-fsm-transition state 'ERROR 'final)
       (error "Unexpected line break (CRLF) inside of an unquoted field"
              field))))))

;; State machine:
;;
;;                 ,---------------------------------------.
;;                 |                                       |
;;                 v                                       |
;;  START ----->[read]--->[join]--->[validate]--->[add]----'
;;          A    | |                    |
;;          |    | |                    `--------> ERROR --.
;;          '----' |                                       |
;;                 |                                       v
;;                 `------------------------------[end]-------> STOP
;;
(define (dsv-string->list/rfc4180 str delimiter)
  (let fold-fields ((record (string-split str delimiter))
                    (buffer '())
                    (prev   '())
                    (state  'read))

    (debug-fsm-transition state)

    (case state

      ((read)
       (let ((field (or (null? record) (car record))))
         (debug-fsm state "field: ~s; buffer: ~s~%" field buffer)
         (cond
          ((null? record)
           (debug-fsm-transition state 'end)
           (fold-fields record buffer prev 'end))
          ((null? buffer)
           (case (get-quotation-status field)
             ((quote-begin quote-begin-or-end)
              (debug-fsm-transition state 'read)
              (fold-fields (cdr record) (cons field buffer) prev 'read))
             (else
              (debug-fsm-transition state 'join)
              (fold-fields (cdr record) (cons field buffer) prev 'join))))
          (else
           (case (get-quotation-status field)
             ((quote-end quote-begin-or-end)
              (debug-fsm-transition state 'join)
              (fold-fields (cdr record) (cons field buffer) prev 'join))
             (else
              (debug-fsm-transition state 'read)
              (fold-fields (cdr record) (cons field buffer) prev 'read)))))))

      ((join)
       (debug-fsm-transition state 'validate)
       (fold-fields record (string-join (reverse buffer) (string delimiter))
                    prev 'validate))

      ((validate)
       (validate-field state buffer)
       (debug-fsm-transition state 'add)
       (fold-fields record buffer prev 'add))

      ((add)
       (let* ((buffer (if (eq? (get-quotation-status buffer) 'quoted)
                          (string-drop-both buffer 1)
                          buffer))
              (buffer (unescape-special-char buffer #\" #\")))
         (debug-fsm-transition state 'read)
         (fold-fields record '() (cons buffer prev) 'read)))

      ((end)
       (debug-fsm-transition state 'STOP 'final)
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

(define* (splice lst-1 lst-2 #:optional (delimiter ""))
  (format #t "splice: lst-1: ~a; lst-2: ~a~%" lst-1 lst-2)
  (cond
   ((null? lst-1)
    lst-2)
   ((null? lst-2)
    lst-1)
   ((and (null? lst-1) (null? lst-2))
    '())
   (else
    (append (drop-right lst-1 1)
            (list (string-append (car (reverse lst-1)) delimiter (car lst-2)))
            (drop lst-2 1)))))


;; XXX: COMMENT-SYMBOL is not used.
;;
;; State machine:
;;
;;                 ,----------------------------------------------------------.
;;                 |            ,---.                                         |
;;                 v            V   |                                         |
;;  START ----->[read-ln]--->[read]-+->[join]----->[validate]--->[add-field]--'
;;                 |  A         |   |                    |
;;                 |  |         |   '->[add-record]-.    `-> ERROR --.
;;                 |  |         |                   |                |
;;                 |  '---------'<------------------'                v
;;                 `--------------------------------------->[end]--------> STOP
;;
(define (dsv-read/rfc4180 port delimiter comment-symbol)
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
       (let ((line (read-line port)))
         (debug-fsm state "line: ~s~%" line)
         (if (not (eof-object? line))
             (begin
               (debug-fsm-transition state 'read)
               (fold-file #:dsv-list     dsv-list
                          #:buffer       buffer
                          #:field-buffer field-buffer
                          #:record       (string-split line delimiter)
                          #:line         line
                          #:state        'read))
             ;; TODO: Handle a premature end of a file (eg. when 'buffer' is
             ;; not null)
             (begin
               (debug-fsm-transition state 'end)
               (fold-file #:dsv-list     dsv-list
                          #:buffer       buffer
                          #:field-buffer field-buffer
                          #:record       record
                          #:line         line
                          #:state        'end)))))

      ((read)
       (let ((field (or (null? record) (car record))))
         (debug-fsm state "field: ~s; field-buffer: ~s~%" field field-buffer)
         (cond
          ((null? record)
           (if (null? field-buffer)
               (begin
                 (debug-fsm-transition state 'add-record)
                 (fold-file #:dsv-list     dsv-list
                            #:buffer       buffer
                            #:field-buffer field-buffer
                            #:record       record
                            #:line         line
                            #:state        'add-record))
               ;; A field contains '\n'.
               ;;   [read]--->[read-ln]
               (begin
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
       (let* ((delimiter  (string delimiter))
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
                                  (reverse field-elements)))))
         (fold-file #:dsv-list     dsv-list
                    #:buffer       buffer
                    #:field-buffer (join-field field-buffer)
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
                        (string-drop-both field-buffer 1)
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

;;; rfc4180.scm ends here

;;; dsv-context.scm -- DSV context for FSMs.

;; Copyright (C) 2023, 2025-2026 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains a context for finite-state machines (FSMs) used in
;; Unix/RFC4180 DSV parsers.
;;
;; The context provides required procedures for the FSMs to work.


;;; Code:

(define-module (dsv fsm dsv-context)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 threads)
  #:use-module (dsv fsm context)
  #:use-module (scheme documentation)
  #:autoload (fibers) (spawn-fiber)
  #:autoload (fibers channels) (make-channel get-message put-message)
  #:export (none
            add-field
            add-non-empty-field
            add-row
            end-of-row?
            delimiter?
            non-printable-character?
            comment-prefix?
            push-non-printable-character
            prepare-result

            <dsv-context>
            make-dsv-context
            dsv-context-update
            dsv-context-next-row
            dsv-context-pre-action
            dsv-context-data-end?
            dsv-context-process-field
            dsv-context-process-row
            dsv-context-process-row/fibers

            rfc4180-writer-quote-field
            rfc4180-writer-process-field
            rfc4180-writer-process-row
            rfc4180-writer-process-row/fibers
            rfc4180-writer-should-be-enclosed?))

(define (none context)
  #f)

(define-with-docs %char-mapping
  "A hash table that holds character mapping for non-printable characters.  The
key is an escaped symbol that represents a non-printable character, and value
is the symbol itself."
  (alist->hash-table
   '((#\f . #\page)
     (#\n . #\newline)
     (#\r . #\return)
     (#\t . #\tab)
     (#\v . #\vtab))))



(define-immutable-record-type <dsv-context>
  ;; This data type is for writing DSV data to a specified port.
  (%make-dsv-context table
                     debug-mode?
                     port
                     delimiter
                     line-break
                     char-mapping
                     row-number
                     row-count
                     custom-data)
  dsv-context?
  ;; DSV data to write.
  ;;
  ;; <list>
  (table        dsv-context-table dsv-context-table-set)
  ;; Is debug mode enabled?
  ;;
  ;; <boolean>
  (debug-mode?  dsv-context-debug-mode? dsv-context-debug-mode-set)
  ;; Output port to write DSV data to.
  ;;
  ;; <port>
  (port         dsv-context-port)
  ;; Output field delimiter.
  ;;
  ;; <string>
  (delimiter    dsv-context-delimiter)
  ;; Line break that must be used in the output data.
  ;;
  ;; <string>
  (line-break   dsv-context-line-break)
  ;; Character mapping for substitution.
  ;;
  ;; <hash-table>
  (char-mapping dsv-context-char-mapping)
  ;; Current row number.
  ;;
  ;; <number>
  (row-number   dsv-context-row-number dsv-context-row-number-set)
  ;; Total number of table row count.
  ;;
  ;; <number>
  (row-count    dsv-context-row-count)

  (custom-data  dsv-context-custom-data))


(define* (make-dsv-context table
                           #:key
                           (debug-mode? #f)
                           (port (current-output-port))
                           delimiter
                           line-break
                           char-mapping
                           (custom-data #f))
  "Make a <dsv-context> instance."
  (%make-dsv-context table
                     debug-mode?
                     port
                     delimiter
                     line-break
                     char-mapping
                     0
                     (length table)
                     custom-data))

(define (dsv-context-update self row)
  "Increment the current row number, drop the first row in the table."
  (let ((self (dsv-context-row-number-set self
                                          (+ (dsv-context-row-number self)
                                             1))))
    (if (null? row)
        self
        (dsv-context-table-set self (cdr (dsv-context-table self))))))

(define (dsv-context-next-row self)
  "Get the first row from a table."
  (let ((data       (dsv-context-table self))
        (row-number (dsv-context-row-number self)))
    (if (< row-number (dsv-context-row-count self))
        (car data)
        '())))

(define (dsv-context-data-end? self row)
  "Check if no data left in the table to process."
  (null? row))



(define (non-printable-character? context char)
  (char? (hash-ref %char-mapping char)))

(define (push-non-printable-character context char)
  (push-event-to-buffer context (hash-ref %char-mapping char)))

(define (end-of-row? context char)
  (or (char:cr? context char)
      (char:lf? context char)))

(define (delimiter? context char)
  (let ((delimiter (hash-ref (context-custom-data context) 'delimiter)))
    (char=? delimiter char)))

(define (comment-prefix? context char)
  (let* ((custom-data (context-custom-data context))
         (prefix      (hash-ref custom-data 'comment-prefix)))
    (cond
     ((char? prefix)
      (char=? prefix char))
     ((string? prefix)
      (char=? (string-ref prefix 0) char))
     (else
      #f))))

(define (context-buffer->string context)
  (list->string (context-buffer/reversed context)))

(define* (add-field context #:optional char)
  (clear-buffer
   (push-event-to-stanza context (context-buffer->string context))))

(define* (add-non-empty-field context #:optional char)
  (if (buffer-empty? context)
      context
      (add-field context)))

(define (throw-row-length-error context row row-length expected-row-length)
  (context-log-error context
                     "Inconsistent length mismatch on line ~a: expected ~a, got ~a"
                     (context-row-number context)
                     expected-row-length
                     row-length)
  (error (format #f
                 "Inconsistent row length on line ~a: expected ~a, got ~a"
                 (context-row-number context)
                 expected-row-length
                 row-length)
         (context-port context)
         (context-row-number context)
         (context-col-number context)
         row
         context))

(define* (add-row context #:optional char)
  (let ((result (context-result context))
        (stanza (context-stanza/reversed context)))
    (if (or (not (hash-ref (context-custom-data context) 'validate?))
            (null? result))
        (clear-stanza
         (push-event-to-result context (context-stanza/reversed context)))
        (let* ((stanza-length   (length stanza))
               (last-row        (car result))
               (last-row-length (length last-row)))
          (if (not (equal? stanza-length last-row-length))
              (throw-row-length-error context
                                      stanza
                                      stanza-length
                                      last-row-length)
              (clear-stanza
               (push-event-to-result context
                                     (context-stanza/reversed context))))))))

(define* (prepare-result context #:optional char)
  (reverse-result context))


;;; Unix writer.

(define (dsv-context-process-field char-mapping field)
  (let loop ((lst (string->list field))
             (result '()))
    (if (not (null? lst))
        (let* ((char (car lst))
               (replacement-char (hash-ref char-mapping char)))
          (loop (cdr lst)
                (if replacement-char
                    (cons replacement-char (cons #\\ result))
                    (cons char result))))
        (reverse result))))

(define (dsv-context-process-row/fibers context row)
  (let ((char-mapping (dsv-context-char-mapping context)))
    (define (proc field)
      (let ((channel (make-channel)))
        (spawn-fiber
         (lambda ()
           (let ((result (dsv-context-process-field char-mapping field)))
             (put-message channel result))))
        channel))
    (let* ((channels (map proc row))
           (results (map (lambda (channel) (list->string (get-message channel)))
                         channels)))
      (display (string-join results (dsv-context-delimiter context))
               (dsv-context-port context))
      (display (dsv-context-line-break context) (dsv-context-port context))
      context)))

(define (dsv-context-process-row context row)
  (let ((char-mapping (dsv-context-char-mapping context)))
    (define (proc field)
      (list->string (dsv-context-process-field char-mapping field)))
    (let ((results (par-map proc row)))
      (display (string-join results (dsv-context-delimiter context))
               (dsv-context-port context))
      (display (dsv-context-line-break context) (dsv-context-port context))
      context)))


;; RFC4180 writer.

(define (rfc4180-writer-quote-field field)
  "Quote a FIELD with double-quotes."
  (append (cons #\" field) '(#\")))

(define (rfc4180-writer-should-be-enclosed? context field)
  "Check if a FIELD should be enclosed in double-quotes."
  (let ((special-chars (dsv-context-custom-data context)))
    (if (or (string-index field special-chars)
            (string-contains field (dsv-context-line-break context)))
        #t
        #f)))

(define (rfc4180-writer-process-field context field)
  (let ((char-mapping (dsv-context-char-mapping context)))
    (let loop ((lst (string->list field))
               (result '()))
      (if (not (null? lst))
          (let* ((char (car lst))
                 (replacement-char (hash-ref char-mapping char)))
            (loop (cdr lst)
                  (if replacement-char
                      (cons replacement-char (cons #\" result))
                      (cons char result))))
          (let ((result (reverse result)))
            (if (rfc4180-writer-should-be-enclosed? context field)
                (rfc4180-writer-quote-field result)
                result))))))

(define (rfc4180-writer-process-row/fibers context row)
  (define (proc field)
    (let ((channel (make-channel)))
      (spawn-fiber
       (lambda ()
         (let ((result (rfc4180-writer-process-field context field)))
           (put-message channel result))))
      channel))
    (let* ((channels (map proc row))
           (results (map (lambda (channel) (list->string (get-message channel)))
                         channels)))
      (display (string-join results (dsv-context-delimiter context))
               (dsv-context-port context))
      (display (dsv-context-line-break context) (dsv-context-port context))
      context))

(define (rfc4180-writer-process-row context row)
  (let ((char-mapping (dsv-context-char-mapping context)))
    (define (proc field)
      (list->string (rfc4180-writer-process-field context field)))
    (let ((results (par-map proc row)))
      (display (string-join results (dsv-context-delimiter context))
               (dsv-context-port context))
      (display (dsv-context-line-break context) (dsv-context-port context))
      context)))

;;; dsv-context.scm ends here.

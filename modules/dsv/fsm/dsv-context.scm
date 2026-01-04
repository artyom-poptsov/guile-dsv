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
  #:use-module (dsv builder)
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

            <unix-writer>
            make-unix-writer
            unix-writer-update
            unix-writer-next-row
            unix-writer-pre-action
            unix-writer-data-end?
            unix-writer-process-field
            unix-writer-process-row
            unix-writer-process-row/fibers))

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


;;; dsv-writer

(define-immutable-record-type <unix-writer>
  (%make-unix-writer table
                     debug-mode?
                     port
                     delimiter
                     line-break
                     char-mapping
                     row-number
                     row-count)
  unix-writer?
  (table       unix-writer-table unix-writer-table-set) ; <list>
  (debug-mode? unix-writer-debug-mode? unix-writer-debug-mode-set) ; <boolean>
  (port        unix-writer-port)          ; <port>
  (delimiter   unix-writer-delimiter)     ; <string>
  (line-break  unix-writer-line-break)    ; <string>
  (char-mapping unix-writer-char-mapping) ; <hash-table.
  (row-number  unix-writer-row-number unix-writer-row-number-set) ; <number>
  (row-count   unix-writer-row-count))                            ; <number>

(define* (make-unix-writer table
                           #:key
                           (debug-mode? #f)
                           (port (current-output-port))
                           delimiter
                           line-break
                           char-mapping)
  "Make a <unix-writer> instance."
  (%make-unix-writer table
                     debug-mode?
                     port
                     delimiter
                     line-break
                     char-mapping
                     0
                     (length table)))

(define (unix-writer-update self row)
  (let ((self (unix-writer-row-number-set self
                                          (+ (unix-writer-row-number self)
                                             1))))
    (if (null? row)
        self
        (unix-writer-table-set self (cdr (unix-writer-table self))))))

(define (unix-writer-next-row context)
  (let ((data       (unix-writer-table context))
        (row-number (unix-writer-row-number context)))
    (if (< row-number (unix-writer-row-count context))
        (car data)
        '())))

(define (unix-writer-data-end? context row)
  (null? row))

(define (unix-writer-process-field char-mapping field)
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

(define (unix-writer-process-row/fibers context row)
  (let ((char-mapping (unix-writer-char-mapping context)))
    (define (proc field)
      (let ((channel (make-channel)))
        (spawn-fiber
         (lambda ()
           (let ((result (unix-writer-process-field char-mapping field)))
             (put-message channel result))))
        channel))
    (let* ((channels (map proc row))
           (results (map (lambda (channel) (list->string (get-message channel)))
                         channels)))
      (display (string-join results (unix-writer-delimiter context))
               (unix-writer-port context))
      (display (unix-writer-line-break context) (unix-writer-port context))
      context)))

(define (unix-writer-process-row context row)
  (let ((char-mapping (unix-writer-char-mapping context)))
    (define (proc field)
      (list->string (unix-writer-process-field char-mapping field)))
    (let ((results (par-map proc row)))
      (display (string-join results (unix-writer-delimiter context))
               (unix-writer-port context))
      (display (unix-writer-line-break context) (unix-writer-port context))
      context)))

;;; dsv-context.scm ends here.

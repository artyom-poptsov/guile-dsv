;;; dsv-context.scm -- DSV context for FSMs.

;; Copyright (C) 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module (dsv fsm context)
  #:export (none
            add-field
            add-non-empty-field
            add-row
            end-of-row?
            delimiter?
            non-printable-character?
            comment-prefix?
            push-non-printable-character
            prepare-result))

(define (none context)
  #f)



(define (non-printable-character? context char)
  (or (char=? char #\f)
      (char=? char #\n)
      (char=? char #\r)
      (char=? char #\t)
      (char=? char #\v)))

(define (push-non-printable-character context char)
  (push-event-to-buffer context
                        (cond
                         ((char=? char #\f) #\page)
                         ((char=? char #\n) #\newline)
                         ((char=? char #\r) #\return)
                         ((char=? char #\t) #\tab)
                         ((char=? char #\v) #\vtab))))

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
                 "Inconsistent row length on line ~a: expected ~a, got ~a; "
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

;;; dsv-context.scm ends here.

;;; common.scm -- Common code for DSV parsers

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

;; Common variables and procedures that are used in DSV parsers.


;;; Code:

(define-module (dsv common)
  #:use-module ((srfi srfi-1) #:select (fold))
  #:use-module (scheme documentation)
  #:export (make-delimiter-guesser

            set-debug! debug debug-fsm debug-fsm-transition debug-fsm-error
            dsv-error

            ;; Variables
            %known-delimiters))


(define-with-docs *debug?*
  "Does debug mode enabled?"
  #f)

(define-with-docs %known-delimiters
  "List of known delimiters"
  '(#\, #\: #\; #\| #\tab #\space))


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

(define (debug-fsm-error state)
  (debug-fsm-transition state 'ERROR 'final))


(define dsv-error
  (case-lambda
    "Throw 'dsv-parser exception with the given MESSAGE and arguments ARGS.
The procedure optionally takes STATE of FSM as the first argument and prints
it as a debug message.."
    ((state message . args)
     (debug-fsm-error state)
     (throw 'dsv-parser-error message args))
    ((message . args)
     (throw 'dsv-parser-error message args))))


(define* (make-delimiter-guesser parser)
  "Make a delimiter guesser that uses a PARSER."
  (let ((get-length (lambda (str d)
                      "Get length of a parsed DSV data.  Return 0 on an error."
                      (catch #t
                        (lambda () (length (car (parser str d))))
                        (const 0)))))
    (lambda* (str #:optional (known-delimiters %known-delimiters))
      "Guess a DSV string STR delimiter.  Optionally accept KNOWN-DELIMITERS
list for guessing."
      (let* ((delimiter-list (map (lambda (d) (cons d (get-length str d)))
                                  known-delimiters))
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
             (caar guessed-delimiter-list))))))

;;; common.scm ends here

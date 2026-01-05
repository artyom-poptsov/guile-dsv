;;; common.scm -- Common code for DSV parsers

;; Copyright (C) 2015-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module ((ice-9 regex) #:select (regexp-substitute/global))
  #:use-module (scheme documentation)
  #:use-module (dsv fsm context)
  #:export (set-debug!
            debug
            dsv-error

            make-delimiter-guesser

            value-or-default
            substitute
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

(define (dsv-error message . args)
  "Throw 'dsv-parser exception with the given MESSAGE and arguments ARGS."
  (throw 'dsv-parser-error message args))


(define (value-or-default value default-value)
  "Return a VALUE if it is not 'default, else return DEFAULT-VALUE."
  (if (eq? value 'default)
      default-value
      value))



(define (make-delimiter-guesser parser-proc fmt)
  (lambda* (str #:key known-delimiters)
    "Guess a DSV string delimiter."
    (and (> (length known-delimiters) 1)
         (let* ((get-length (lambda (d)
                              (catch #t
                                (lambda ()
                                  (length (car (parser-proc str
                                                            #:delimiter d))))
                                (const 0))))
                (delimiter-list (map (lambda (d) (cons d (get-length d)))
                                     known-delimiters))
                (guessed-delimiter-list
                 (fold (lambda (a prev)
                         (if (not (null? prev))
                             (let ((a-count (cdr a))
                                   (b-count (cdar prev)))
                               (cond ((> a-count b-count) (list a))
                                     ((= a-count b-count) (append (list a)
                                                                  prev))
                                     (else prev)))
                             (list a)))
                       '()
                       delimiter-list)))
           (and (= (length guessed-delimiter-list) 1)
                (caar guessed-delimiter-list))))))

;;; common.scm ends here

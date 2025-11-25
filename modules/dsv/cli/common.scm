;;; common.scm -- Common Guile-DSV CLI code.

;; Copyright (C) 2021-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Common Guile-DSV CLI code.


;;; Code:

(define-module (dsv cli common)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
  #:use-module (srfi  srfi-1)
  #:use-module (srfi  srfi-26)
  #:use-module (system foreign)
  #:use-module (dsv)
  #:use-module (dsv table)
  #:use-module (dsv config)
  #:use-module (dsv table-preset)
  #:export (terminal-size
            string->dsv-format
            borders->alist
            guess-file-delimiter
            print-delimiter
            print-file
            print-summary
            remove-empty-rows
            rc
            convert))



(define (terminal-size)
  "Get the terminal size. Return the number rows (lines) and columns as a pair
of numbers, or #f if an error occurred."
  (catch #t
    (lambda ()
      (let* ((p      (open-input-pipe "stty -F /dev/tty size"))
             (result (read-line p)))
        (and (not (eof-object? result))
             (let* ((data (map string->number (string-split result #\space)))
                    (rows (car data))
                    (cols (cadr data)))
               (cons rows cols)))))
    (lambda (key . args)
      (format (current-error-port)
              "ERROR: Could not get the terminal size: ~a: ~a"
              key args)
      #f)))



(define (string->dsv-format str)
  "Convert a string STR to a DSV format type."
  (let ((fmt (string->symbol str)))
    (case fmt
      ((unix rfc4180)
       fmt)
      (else
       (error "Wrong format" str)))))

(define (borders->alist borders)
  "Convert BORDERS spec to an alist."
  (if (not (string-null? borders))
      (map (lambda (s)
             (let ((lst (string-split s #\=)))
               (if (= (length lst) 1)
                   (car lst)
                   (cons (shorthand->table-parameter (string->symbol (car lst)))
                         (cadr lst)))))
           (car (dsv-string->scm borders #\,)))
      '()))



(define (guess-file-delimiter input-port fmt)
  "Guess delimiter for a file."
  (let guess ((line (read-line input-port 'concat))
              (buf  '()))
    (and (not (eof-object? line))
         (let ((guessed-delim (guess-delimiter line #:format fmt)))
           (if guessed-delim
               (begin
                 (for-each (lambda (str)
                             (unget-string input-port str))
                           (cons line buf))
                 guessed-delim)
               (guess (read-line input-port)
                      (cons line buf)))))))

(define (print-delimiter input-port fmt)
  "Guess and print FILE delimiter."
  (let ((guessed-delim (guess-file-delimiter input-port fmt)))
    (format #t "~a~%" (or guessed-delim ""))))



(define (remove-empty-rows table)
  (remove (lambda (f)
            (and (= (length f) 1)
                 (string-null? (car f))))
          table))

(define (rc file delim fmt)
  "Get number of records in a FILE."
  (let ((p (open-input-file file)))
    (length (dsv->scm p delim #:format fmt))))


(define* (print-file input-port fmt borders delim
                     #:key
                     (numbering?      #f)
                     (width           #f)
                     (with-header?    #f)
                     (filter-row-proc #f)
                     (filter-col-proc #f)
                     (proc            #f)
                     (debug-mode?     #f)
                     (table-presets-path %table-presets-path))
  "Pretty-print a FILE."
  (let ((delim (or delim (guess-file-delimiter input-port fmt))))

    (unless delim
      (error "Could not determine a file delimiter" input-port))

    (let* ((table (remove-empty-rows (dsv->scm input-port delim #:format fmt
                                               #:validate? #t
                                               #:debug-mode? debug-mode?)))
           (table (if filter-col-proc
                      (table-filter-column filter-col-proc table)
                      table))
           (table (if proc
                      (table-map proc
                                 (if filter-row-proc
                                     (table-filter-row filter-row-proc table)
                                     table))
                      (if filter-row-proc
                          (table-filter-row filter-row-proc table)
                          table)))
           (table  (if numbering?
                       (table-number table)
                       table))
           (bspec (let ((borders (borders->alist borders)))
                    (if (and (not (null? borders))
                             (string? (car borders))
                             (table-preset-name? (car borders)))
                        (table-preset-override
                         (load-table-preset (car borders)
                                            #:table-presets-path table-presets-path)
                         (cdr borders))
                        borders))))
      (format-table table bspec #:with-header? with-header? #:width width))))

(define (print-summary input-port fmt delim)
  "Print summary information for an INPUT-PORT of format FMT."
  (let ((guessed-delim (or delim (guess-file-delimiter input-port fmt)))
        (f             (cut format #t <...>)))

    (f "File:      ~a~%" (or (port-filename input-port)
                             ""))
    (f "Format:    ~a~%" fmt)
    (f "Delimiter: '~a'~a~%"
       (or guessed-delim "")
       (if guessed-delim
           (format #f " (0x~x)" (char->integer guessed-delim))
           ""))

    (when guessed-delim
      (let* ((table  (remove-empty-rows
                      (dsv->scm input-port guessed-delim
                                #:format fmt)))
             (wtable (let loop ((w   (get-width table))
                                (col 1)
                                (res '()))
                       (if (not (null? w))
                           (loop (cdr w)
                                 (1+ col)
                                 (cons (list (number->string col)
                                             (number->string (car w)))
                                       res))
                           (reverse res)))))
        (f "Records:   ~a~%" (length table))
        (newline)
        (format-table (cons '("column" "width") wtable) '())))))



(define* (convert input-port
                  #:key
                  source-delimiter
                  target-delimiter
                  source-format
                  target-format
                  (numbering? #f)
                  (filter-col-proc #f)
                  (filter-row-proc #f)
                  (proc #f)
                  (debug-mode? #f))
  "Convert a data from an INPUT-PORT from a SOURCE-FORMAT to a TARGET-FORMAT.
Optionally filter each row, column and cell with FILTER-ROW-PROC,
FILTER-COL-PROC and PROC respectively, if those procedures are specified."
  (case target-format
    ((unix rfc4180)
     (let ((source-delimiter (or source-delimiter
                                 (guess-file-delimiter input-port source-format)))
           (target-delimiter (or target-delimiter 'default)))
       (unless source-delimiter
         (error "Could not determine a file delimiter" input-port))
       (let* ((table (remove-empty-rows (dsv->scm input-port source-delimiter
                                                  #:debug-mode? debug-mode?
                                                  #:format source-format)))
              (table (if filter-col-proc
                         (table-filter-column filter-col-proc table)
                         table))
              (table (if proc
                         (table-map proc
                                    (if filter-row-proc
                                        (table-filter-row filter-row-proc table)
                                        table))
                         (if filter-row-proc
                             (table-filter-row filter-row-proc table)
                             table)))
              (table  (if numbering?
                          (table-number table)
                          table)))
         (scm->dsv table (current-output-port) target-delimiter
                   #:format target-format))))
    (else
     (error "Unsupported target format" target-format))))

;;; common.scm ends here.

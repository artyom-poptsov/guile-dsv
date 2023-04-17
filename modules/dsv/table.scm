;;; table.scm -- Procedures to print fancy tables in a console.

;; Copyright (C) 2021-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; This module contains procedures that allow to print configurable fancy
;; tables in a console.


;;; Code:

(define-module (dsv table)
  #:use-module (scheme documentation)
  #:use-module (ice-9 format)
  #:export (%table-parameters
            string*
            table-print-element
            print-table-parameters
            shorthand->table-parameter
            get-width
            format-table
            table-format-field
            table-map
            table-filter-row
            table-filter-column))

(define-with-docs %table-parameters
  "Associative list of all known table parameters."
  '((bt  . border-top)
    (btl . border-top-left)
    (btr . border-top-right)
    (btj . border-top-joint)
    (bl  . border-left)
    (blj . border-left-joint)
    (br  . border-right)
    (brj . border-right-joint)
    (bb  . border-bottom)
    (bbl . border-bottom-left)
    (bbr . border-bottom-right)
    (bbj . border-bottom-joint)
    ;; Shadow.
    (s   . shadow)
    (so  . shadow-offset)
    ;; Inner table lines.
    (rs  . row-separator)
    (rj  . row-joint)
    (cs  . column-separator)
    ;; Headers.
    (ht  . header-top)
    (htl . header-top-left)
    (htr . header-top-right)
    (htj . header-top-joint)
    (hl  . header-left)
    (hr  . header-right)
    (hcs . header-column-separator)
    (hb  . header-bottom)
    (hbl . header-bottom-left)
    (hbr . header-bottom-right)
    (hbj . header-bottom-joint)))

(define (print-table-parameters port)
  "Print all known table parameters to a PORT."
  (for-each (lambda (param)
              (format port
                      "  ~4a ~a~%"
                      (car param)
                      (cdr param)))
            %table-parameters))

(define (shorthand->table-parameter sh)
  "Convert a shorthand SH to a table parameter."
  (let ((param (assoc-ref %table-parameters sh)))
    (unless param
      (format (current-error-port)
              "ERROR: Unknown table parameter: ~a~%" sh)
      (format (current-error-port)
              "Known table parameters are:~%")
      (print-table-parameters (current-error-port))
      (exit 1))
    param))



(define (get-width table)
  "Get maximum field width for each row of TABLE."
  (let loop ((rows table)
             (res  '()))
    (if (not (null? rows))
        (let ((w (map string-length (car rows))))
          (cond
           ((null? res)
            (loop (cdr rows) w))
           (else
            (loop (cdr rows)
                  (map max res w)))))
        res)))

(define (table-map proc table)
  "Apply a procedure PROC to each TABLE cell, return the new table."
  (define (process-row row row-num)
    (let loop ((r       row)
               (col-num 0)
               (result  '()))
      (if (null? r)
          (reverse result)
          (loop (cdr r)
                (+ col-num 1)
                (cons (proc (car r)
                            row-num
                            col-num)
                      result)))))

  (let row-loop ((tbl     table)
                 (row-num 0)
                 (result  '()))
    (if (null? tbl)
        (reverse result)
        (row-loop (cdr tbl)
                  (+ row-num 1)
                  (cons (process-row (car tbl)
                                     row-num)
                        result)))))

(define (table-filter-row proc table)
  (let loop ((tbl     table)
             (row-num 0)
             (result  '()))
    (if (null? tbl)
        (reverse result)
        (let ((row (car tbl)))
          (if (proc row row-num)
              (loop (cdr tbl)
                    (+ row-num 1)
                    (cons row result))
              (loop (cdr tbl)
                    (+ row-num 1)
                    result))))))

(define (table-filter-column proc table)
  "Remove all the columns from a TABLE for which a procedure PROC returns #f."
  (let ((first-row-length (length (car table))))
    (let loop ((col    0)
               (result (make-list (length table) '())))
      (if (= col first-row-length)
          (map reverse result)
          (let ((value (map (lambda (r) (list-ref r col)) table)))
            (if (proc value col)
                (loop (+ col 1)
                      (map cons value result))
                (loop (+ col 1)
                      result)))))))



(define* (table-format-field field width
                             #:key
                             (padding 0)
                             (port (current-output-port)))
  "Print a FIELD to a PORT in a column with given WIDTH and PADDING."
  (format port (format #f " ~~~da " (+ width padding)) field))

(define* (string* str k)
  "Return a newly allocated string that is made from K instances of a string
STR."
  (string-join (make-list k str) ""))

(define (table-print-element element port)
  "Print a table ELEMENT to a PORT, or a single space if element is #f."
  (display (if element element " ") port))

(define* (format-table table
                       borders
                       #:key
                       (with-header? #f)
                       (port (current-output-port)))
  "Format file and print it to a PORT."
  (let* ((padding 5)
         (column-separator    (or (assoc-ref borders 'column-separator) ""))
         (row-separator       (assoc-ref borders 'row-separator))
         (row-joint           (assoc-ref borders 'row-joint))
         (border-top          (assoc-ref borders 'border-top))
         (border-top-left     (assoc-ref borders 'border-top-left))
         (border-top-right    (assoc-ref borders 'border-top-right))
         (border-top-joint    (assoc-ref borders 'border-top-joint))
         (border-left         (assoc-ref borders 'border-left))
         (border-left-joint   (assoc-ref borders 'border-left-joint))
         (border-right        (assoc-ref borders 'border-right))
         (border-right-joint  (assoc-ref borders 'border-right-joint))
         (border-bottom       (assoc-ref borders 'border-bottom))
         (border-bottom-left  (assoc-ref borders 'border-bottom-left))
         (border-bottom-right (assoc-ref borders 'border-bottom-right))
         (border-bottom-joint (assoc-ref borders 'border-bottom-joint))
         (header-top          (assoc-ref borders 'header-top))
         (header-top-left     (assoc-ref borders 'header-top-left))
         (header-top-right    (assoc-ref borders 'header-top-right))
         (header-top-joint    (assoc-ref borders 'header-top-joint))
         (header-left         (assoc-ref borders 'header-left))
         (header-right        (assoc-ref borders 'header-right))
         (header-column-separator (assoc-ref borders
                                             'header-column-separator))
         (header-bottom       (assoc-ref borders 'header-bottom))
         (header-bottom-left  (assoc-ref borders 'header-bottom-left))
         (header-bottom-right (assoc-ref borders 'header-bottom-right))
         (header-bottom-joint (assoc-ref borders 'header-bottom-joint))
         (shadow              (assoc-ref borders 'shadow))
         (shadow-offset       (assoc-ref borders 'shadow-offset))
         (shadow-offset       (and shadow-offset
                                   (map string->number
                                        (string-split shadow-offset #\:))))
         (shadow-x-offset     (and shadow-offset
                                   (car shadow-offset)))
         (shadow-y-offset     (and shadow-offset
                                   (cadr shadow-offset)))
         (width        (get-width table))
         (format-row   (lambda* (row width border-left border-right separator
                                     #:key (row-number 0))
                         (when (and shadow
                                    shadow-offset
                                    (< shadow-x-offset 0))
                           (if (or (not row-number)
                                   (> row-number shadow-y-offset))
                               (display (string* shadow (abs shadow-x-offset))
                                        port)
                               (display (string* " " (abs shadow-x-offset))
                                        port)))
                         (table-print-element border-left port)
                         (let field-loop ((fields       row)
                                          (field-widths width))
                           (unless (null? fields)
                             (let ((f (car fields))
                                   (w (car field-widths)))
                               (table-format-field f w
                                                   #:padding padding
                                                   #:port port)
                               (if (null? (cdr fields))
                                   (table-print-element border-right port)
                                   (table-print-element separator port))
                               (field-loop (cdr fields) (cdr field-widths)))))
                         (when (and shadow
                                    shadow-offset
                                    (> shadow-x-offset 0))
                           (if (> row-number shadow-y-offset)
                               (display (string* shadow shadow-x-offset)
                                        port)
                               (display (string* " " shadow-x-offset)
                                        port)))
                         (newline port)))
         (display-line (lambda* (widths middle left right joint
                                        #:key
                                        (row-number #f)
                                        (port port))
                         (when (and shadow
                                    shadow-offset
                                    (< shadow-x-offset 0))
                           (if (or (not row-number)
                                   (> row-number shadow-y-offset))
                               (display (string* shadow (abs shadow-x-offset))
                                        port)
                               (display (string* " " (abs shadow-x-offset))
                                        port)))
                         (table-print-element left port)
                         (let loop ((w widths))
                           (unless (null? w)
                             (let ((row-width (+ (car w) padding 2)))
                               (display (string* middle row-width)
                                        port)
                               (unless (null? (cdr w))
                                 (table-print-element joint port)))
                             (loop (cdr w))))
                         (table-print-element right port)
                         (when (and shadow
                                    shadow-offset
                                    (> shadow-x-offset 0))
                           (if (or (not row-number)
                                   (> row-number shadow-y-offset))
                               (display (string* shadow shadow-x-offset)
                                        port)
                               (display (string* " " shadow-x-offset)
                                        port)))
                         (newline port)))
         (display-header-border-top
          (lambda (widths)
            (display-line widths
                          header-top
                          header-top-left
                          header-top-right
                          header-top-joint
                          #:row-number 0)))
         (display-header-border-bottom
          (lambda (widths)
            (display-line widths
                          header-bottom
                          header-bottom-left
                          header-bottom-right
                          header-bottom-joint
                          #:row-number 2)))
         (display-top-border (lambda (widths)
                               "Display a top horisontal table border."
                               (display-line widths
                                             border-top
                                             border-top-left
                                             border-top-right
                                             border-top-joint
                                             #:row-number 0)))
         (display-bottom-border (lambda (widths row-number)
                                  "Display a top horisontal table border."
                                  (display-line widths
                                                border-bottom
                                                border-bottom-left
                                                border-bottom-right
                                                border-bottom-joint
                                                #:row-number row-number)))
         (display-row-separator (lambda (widths row-number)
                                  "Display a top horisontal table border."
                                  (display-line widths
                                                row-separator
                                                border-left-joint
                                                border-right-joint
                                                row-joint
                                                #:row-number row-number)))
         (display-table (lambda (table)
                          (unless with-header?
                            (when border-top
                              (display-top-border width)))
                          (let loop ((t table)
                                     (row-number (if (and with-header?
                                                          border-top)
                                                     2
                                                     1)))
                            (unless (null? t)
                              (format-row (car t)
                                          width
                                          border-left
                                          border-right
                                          column-separator
                                          #:row-number row-number)
                              (when row-separator
                                (if (null? (cdr t))
                                    (when border-bottom
                                      (display-bottom-border width row-number))
                                    (display-row-separator width (+ row-number 1))))
                              (loop (cdr t) (+ row-number 2)))))))

    (when (and shadow
               shadow-offset
               (< shadow-y-offset 0))
      (let ((str (with-output-to-string
                   (lambda ()
                     (display-line width
                                   shadow
                                   shadow
                                   shadow
                                   shadow
                                   #:port (current-output-port))))))
        (for-each (lambda (i)
                    (display (string* " " shadow-x-offset)
                             port)
                    (display (string-copy str shadow-x-offset) port))
                  (iota (abs shadow-y-offset)))))

    (if with-header?
        (begin
          (when header-top
            (display-header-border-top width))
          (format-row (car table)
                      width
                      header-left
                      header-right
                      header-column-separator
                      #:row-number 2)
          (when header-bottom
            (display-header-border-bottom width))
          (display-table (cdr table)))
        (display-table table))

    (when (and shadow shadow-offset)
        (when (> shadow-y-offset 0)
          (let ((str (with-output-to-string
                       (lambda ()
                         (display-line width
                                       shadow
                                       shadow
                                       shadow
                                       shadow
                                       #:port (current-output-port))))))
            (for-each (lambda (i)
                        (if (>= shadow-x-offset 0)
                            (begin
                              (display (string* " " shadow-x-offset)
                                       port)
                              (display (string-copy str (abs shadow-x-offset)) port))
                            (begin
                              (display (string* shadow (abs shadow-x-offset))
                                       port)
                              (display str port))))
                      (iota shadow-y-offset)))))))

;;; table.scm ends here.

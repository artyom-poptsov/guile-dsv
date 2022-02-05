;;; table.scm -- Procedures to print fancy tables in a console.

;; Copyright (C) 2021-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
            print-table-parameters
            shorthand->table-parameter
            get-width
            format-table
            table-map
            filter-row
            filter-column))

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

(define (filter-row table proc)
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

(define (filter-column table proc)
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
         (width        (get-width table))
         (format-field (lambda (field width)
                         "Print a FIELD in a column with given WIDTH."
                         (format port
                                 (format #f " ~~~da " (+ width padding))
                                 field)))
         (format-row   (lambda (row width border-left border-right separator)
                         (if border-left
                             (display border-left port)
                             (display " " port))
                         (let field-loop ((fields       row)
                                          (field-widths width))
                           (unless (null? fields)
                             (let ((f (car fields))
                                   (w (car field-widths)))
                               (format-field f w)
                               (if (null? (cdr fields))
                                   (if border-right
                                       (display border-right port)
                                       (display " " port))
                                   (if separator
                                       (display separator port)
                                       (display " " port)))
                               (field-loop (cdr fields) (cdr field-widths)))))
                         (newline port)))
         (display-line (lambda (widths middle left right joint)
                         (if left
                             (display left port)
                             (display " " port))
                         (let loop ((w widths))
                           (unless (null? w)
                             (let ((row-width (+ (car w) padding 2)))
                               (display (string-join (make-list row-width
                                                                middle)
                                                     "")
                                        port)
                               (unless (null? (cdr w))
                                 (if joint
                                     (display joint port)
                                     (display " " port))))
                             (loop (cdr w))))
                         (if right
                             (display right port)
                             (display " " port))
                         (newline port)))
         (display-header-border-top
          (lambda (widths)
            (display-line widths
                          header-top
                          header-top-left
                          header-top-right
                          header-top-joint)))
         (display-header-border-bottom
          (lambda (widths)
            (display-line widths
                          header-bottom
                          header-bottom-left
                          header-bottom-right
                          header-bottom-joint)))
         (display-top-border (lambda (widths)
                               "Display a top horisontal table border."
                               (display-line widths
                                             border-top
                                             border-top-left
                                             border-top-right
                                             border-top-joint)))
         (display-bottom-border (lambda (widths)
                                  "Display a top horisontal table border."
                                  (display-line widths
                                                border-bottom
                                                border-bottom-left
                                                border-bottom-right
                                                border-bottom-joint)))
         (display-row-separator (lambda (widths)
                                  "Display a top horisontal table border."
                                  (display-line widths
                                                row-separator
                                                border-left-joint
                                                border-right-joint
                                                row-joint)))
         (display-table (lambda (table)
                          (unless with-header?
                            (when border-top
                              (display-top-border width)))
                          (let loop ((t table))
                            (unless (null? t)
                              (format-row (car t)
                                          width
                                          border-left
                                          border-right
                                          column-separator)
                              (when row-separator
                                (if (null? (cdr t))
                                    (when border-bottom
                                      (display-bottom-border width))
                                    (display-row-separator width)))
                              (loop (cdr t)))))))
    (if with-header?
        (begin
          (when header-top
            (display-header-border-top width))
          (format-row (car table)
                      width
                      header-left
                      header-right
                      header-column-separator)
          (when header-bottom
            (display-header-border-bottom width))
          (display-table (cdr table)))
        (display-table table))))

;;; table.scm ends here.

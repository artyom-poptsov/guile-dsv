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
  #:use-module (srfi srfi-1)
  #:export (%table-parameters
            string*
            stylize
            string-slice
            table-format-row
            table-wrap-row
            table-wrap
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
  '((bt    border-top          "The top border.")
    (btl   border-top-left     "The top left corner.")
    (btr   border-top-right    "The top right corner.")
    (btj   border-top-joint    "The top border joint.")
    (bl    border-left         "The left table border.")
    (blj   border-left-joint   "The left table border joint.")
    (br    border-right        "The right table border.")
    (brj   border-right-joint  "The right table border joint.")
    (bb    border-bottom       "The bottom border.")
    (bbl   border-bottom-left  "The left corner of the bottom border.")
    (bbr   border-bottom-right "The right corner of the bottom border.")
    (bbj   border-bottom-joint "The bottom border joint.")
    (bs    border-style        "The style of the borders (\"fg;bg\".)")
    (ts    text-style          "The text style (\"fg;bg\".)")
    ;; Shadow.
    (s     shadow              "The table shadow.")
    (so    shadow-offset       "The table shadow offset in format \"x;y\" (e.g. \"2;2\".)")
    (ss    shadow-style        "The style of the shadow (\"fg;bg\".)")
    ;; Inner table lines.
    (rs    row-separator       "The table row separator.")
    (rj    row-joint           "The row joint.")
    (cs    column-separator    "The table column separator")
    ;; Headers.
    (hs    header-style        "The header style (\"fg;bg\".)")
    (ht    header-top          "The header top border.")
    (htl   header-top-left     "The header top left border.")
    (htr   header-top-right    "The header top right border.")
    (htj   header-top-joint    "The header top joint.")
    (hl    header-left         "The header left border.")
    (hr    header-right        "The header right border.")
    (hcs   header-column-separator "The header column separator.")
    (hb    header-bottom       "The header bottom border.")
    (hbl   header-bottom-left  "The header bottom left corner.")
    (hbr   header-bottom-right "The header bottom right border.")
    (hbj   header-bottom-joint "The header bottom joint.")))

(define (print-table-parameters port)
  "Print all known table parameters to a PORT."
  (for-each (lambda (param)
              (let ((param-shorthand   (car param))
                    (param-long        (cadr param))
                    (param-description (caddr param)))
                (format port
                        "  ~4a ~25a ~a~%"
                        param-shorthand
                        param-long
                        param-description)))
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
    (car param)))



(define (get-width table)
  "Get maximum field width for each row of TABLE."
  (let loop ((rows table)
             (res  '()))
    (if (not (null? rows))
        (let ((w (map (lambda (e)
                        (cond
                         ((list? e)
                          (string-length (car e)))
                         (else
                          (string-length e))))
                      (car rows))))
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
                             (padding 0))
  "Print a FIELD in a column with given WIDTH and PADDING.  Return a string."
  (format #f (format #f " ~~~da " (+ width padding)) field))

(define* (string* str k)
  "Return a newly allocated string that is made from K instances of a string
STR."
  (string-join (make-list k str) ""))

(define (stylize str style)
  (if style
      (format #f
              "[~am~a[0m"
              style
              str)
      str))

(define (string-slice s width)
  "Slice a string S into parts of WIDTH length.  Return the list of strings."
  (if (zero? width)
      (list s)
      (let ((slen (string-length s)))
        (let loop ((idx    0)
                   (result '()))
          (if (>= (+ idx width) slen)
              (reverse (cons (string-copy s idx slen) result))
              (loop (+ idx width)
                    (cons (string-copy s idx (+ idx width)) result)))))))

(define (table-wrap-row row widths)
  "Wrap a table ROW to fit each cell it into the specified WIDTHS.  Return a
list where each row is represented as a sub-list of strings."
  (let loop ((r row)
             (w widths)
             (result '()))
    (if (null? r)
        (reverse result)
        (let* ((cell   (car r))
               (cell-w (car w))
               (new-cell (string-slice cell cell-w)))
          (loop (cdr r)
                (cdr w)
                (cons new-cell result))))))

(define (table-group wrapped-strings)
  (let loop ((data wrapped-strings)
             (result '()))
    (let* ((row (map (lambda (col)
                       (if (null? col)
                           ""
                           (car col)))
                     data))
           (new-data (map (lambda (col)
                            (if (null? col)
                                '()
                                (cdr col)))
                          data))
           (has-next? (find (lambda (col)
                              (not (null? col)))
                            new-data)))
      (if has-next?
          (loop new-data (cons row result))
          (reverse (cons row result))))))

(define* (table-wrap table
                     current-column-widths
                     #:key
                     (width 80)
                     (padding 0))
  (let* ((column-count (length (car table)))
         (current-total-width (fold + 0 current-column-widths))
         (percents (map (lambda (w)
                          (* (/ w current-total-width) 100.0))
                        current-column-widths))
         (new-widths (map (lambda (p)
                            (inexact->exact (ceiling (* (/ p 100.0) width))))
                          percents)))
    (let loop ((old-table table)
               (new-table '()))
      (if (null? old-table)
          (reverse new-table)
          (let* ((old-row  (car old-table))
                 (new-rows (table-wrap-row old-row new-widths)))
            (loop (cdr old-table)
                  (append (list new-rows) new-table)))))))

(define (table-print-element element port)
  "Print a table ELEMENT to a PORT, or a single space if element is #f."
  (display (if element element " ") port))

(define* (table-format-row row
                           #:key
                           (row-number 0)
                           (borders '())
                           (cell-widths '())
                           (type 'body)
                           (separator "")
                           (padding 0)
                           (port (current-output-port)))
  (let* ((border-left  (assoc-ref borders 'border-left))
         (border-right (assoc-ref borders 'border-right))
         (shadow (assoc-ref borders 'shadow))
         (shadow-style        (assoc-ref borders 'shadow-style))
         (shadow-offset       (assoc-ref borders 'shadow-offset))
         (shadow-offset       (and shadow-offset
                                   (map string->number
                                        (string-split shadow-offset #\;))))
         (border-style        (assoc-ref borders 'border-style))
         (text-style          (assoc-ref borders 'text-style))
         (header-style        (assoc-ref borders 'header-style))
         (header-left         (assoc-ref borders 'header-left))
         (header-right        (assoc-ref borders 'header-right))
         (shadow-x-offset     (and shadow-offset
                                   (car shadow-offset)))
         (shadow-y-offset     (and shadow-offset
                                   (cadr shadow-offset))))
    (when (and shadow
               shadow-offset
               (< shadow-x-offset 0))
      (if (or (not row-number)
              (>= row-number shadow-y-offset))
          (display (stylize
                    (string* shadow (abs shadow-x-offset))
                    shadow-style)
                   port)
          (display (string* " " (abs shadow-x-offset))
                   port)))
    (table-print-element (stylize (if (equal? type 'body)
                                      border-left
                                      header-left)
                                  border-style)
                         port)
    (let field-loop ((fields       row)
                     (field-widths cell-widths))
      (unless (null? fields)
        (let ((f (car fields))
              (w (car field-widths)))
          (display (stylize
                    (table-format-field f w
                                        #:padding padding)
                    (if (equal? type 'body)
                        text-style
                        (if header-style
                            header-style
                            text-style)))
                   port)
          (if (null? (cdr fields))
              (table-print-element (stylize (if (equal? type 'body)
                                                border-right
                                                header-right)
                                            border-style)
                                   port)
              (table-print-element (stylize separator
                                            border-style)
                                   port))
          (field-loop (cdr fields) (cdr field-widths)))))
    (when (and shadow
               shadow-offset
               (> shadow-x-offset 0))
      (if (> row-number shadow-y-offset)
          (display (stylize
                    (string* shadow shadow-x-offset)
                    shadow-style)
                   port)
          (display (string* " " shadow-x-offset)
                   port)))
    (newline port)))


(define* (format-table table
                       borders
                       #:key
                       (width #f)
                       (with-header? #f)
                       (port (current-output-port)))
  "Format file and print it to a PORT."
  (let* ((padding 2)
         (table  (if (and width (not (zero? width)))
                     (table-wrap table (get-width table) #:width width)
                     table))
         (row-widths        (get-width table))
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
         (border-style        (assoc-ref borders 'border-style))
         (header-style        (assoc-ref borders 'header-style))
         (header-bottom       (assoc-ref borders 'header-bottom))
         (header-bottom-left  (assoc-ref borders 'header-bottom-left))
         (header-bottom-right (assoc-ref borders 'header-bottom-right))
         (header-bottom-joint (assoc-ref borders 'header-bottom-joint))
         (shadow              (assoc-ref borders 'shadow))
         (shadow-style        (assoc-ref borders 'shadow-style))
         (shadow-offset       (assoc-ref borders 'shadow-offset))
         (shadow-offset       (and shadow-offset
                                   (map string->number
                                        (string-split shadow-offset #\;))))
         (shadow-x-offset     (and shadow-offset
                                   (car shadow-offset)))
         (shadow-y-offset     (and shadow-offset
                                   (cadr shadow-offset)))
         (text-style          (assoc-ref borders 'text-style))
         (display-line (lambda* (widths middle left right joint
                                        #:key
                                        (row-number #f)
                                        (port port))
                         (when (and shadow
                                    shadow-offset
                                    (<= shadow-x-offset 0))
                           (if (or (not row-number)
                                   (> row-number shadow-y-offset))
                               (display (stylize (string* shadow (abs shadow-x-offset))
                                                 shadow-style)
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
                                    (>= shadow-x-offset 0))
                           (if (or (not row-number)
                                   (> row-number shadow-y-offset))
                               (display (stylize (string* shadow shadow-x-offset)
                                                 shadow-style)
                                        port)
                               (display (string* " " shadow-x-offset)
                                        port)))
                         (newline port)))
         (display-header-border-top
          (lambda (widths)
            (display-line widths
                          (stylize header-top border-style)
                          (stylize header-top-left border-style)
                          (stylize header-top-right border-style)
                          (stylize header-top-joint border-style)
                          #:row-number 0)))
         (display-header-border-bottom
          (lambda (widths)
            (display-line widths
                          (stylize header-bottom border-style)
                          (stylize header-bottom-left border-style)
                          (stylize header-bottom-right border-style)
                          (stylize header-bottom-joint border-style)
                          #:row-number 2)))
         (display-top-border (lambda (widths)
                               "Display a top horisontal table border."
                               (display-line widths
                                             (stylize border-top border-style)
                                             (stylize border-top-left border-style)
                                             (stylize border-top-right border-style)
                                             (stylize border-top-joint border-style)
                                             #:row-number 0)))
         (display-bottom-border (lambda (widths row-number)
                                  "Display a top horisontal table border."
                                  (display-line widths
                                                (stylize border-bottom border-style)
                                                (stylize border-bottom-left border-style)
                                                (stylize border-bottom-right border-style)
                                                (stylize border-bottom-joint border-style)
                                                #:row-number row-number)))
         (display-row-separator (lambda (widths row-number)
                                  "Display a top horisontal table border."
                                  (display-line widths
                                                (stylize row-separator border-style)
                                                (stylize border-left-joint border-style)
                                                (stylize border-right-joint border-style)
                                                (stylize row-joint border-style)
                                                #:row-number row-number)))
         (display-table (lambda (table)
                          (define (write-separator t row-number)
                            (when row-separator
                              (if (null? (cdr t))
                                  (when border-bottom
                                    (display-bottom-border row-widths row-number))
                                  (display-row-separator row-widths (+ row-number 1)))))
                          (unless with-header?
                            (when border-top
                              (display-top-border row-widths)))
                          (let loop ((t table)
                                     (row-number (if (and with-header?
                                                          border-top)
                                                     3
                                                     1)))
                            (unless (null? t)
                              (let ((row (car t)))
                                (if (list? (car row))
                                    (let lp ((r row)
                                             (rnum row-number))
                                      (if (not (find (lambda (c)
                                                       (not (null? c)))
                                                     r))
                                          (begin
                                            (write-separator t rnum)
                                            (loop (cdr t) (+ rnum 2)))
                                          (let ((part (map (lambda (e)
                                                             (cond
                                                              ((null? e)
                                                               "")
                                                              (else
                                                               (car e))))
                                                             r)))
                                            (table-format-row part
                                                              #:cell-widths row-widths
                                                              #:borders borders
                                                              #:separator column-separator
                                                              #:row-number rnum
                                                              #:padding padding
                                                              #:port port)
                                            (lp (map (lambda (e)
                                                       (cond
                                                        ((null? e)
                                                         '())
                                                        (else
                                                         (cdr e))))
                                                     r)
                                                (+ rnum 1)))))
                                    (begin
                                      (table-format-row (car t)
                                                        #:cell-widths row-widths
                                                        #:borders borders
                                                        #:separator column-separator
                                                        #:row-number row-number
                                                        #:padding padding
                                                        #:port port)
                                      (write-separator t row-number)
                                      (loop (cdr t) (+ row-number 2))))))))))

    (when (and shadow
               shadow-offset
               (< shadow-y-offset 0))
      (let ((str (with-output-to-string
                   (lambda ()
                     (display-line row-widths
                                   shadow
                                   shadow
                                   shadow
                                   shadow
                                   #:port (current-output-port))))))
        (for-each (lambda (i)
                    (display (string* " " shadow-x-offset)
                             port)
                    (display (stylize (string-copy str shadow-x-offset) shadow-style)
                             port))
                  (iota (abs shadow-y-offset)))))

    (if with-header?
        (let ((header (car table)))
          (when header-top
            (display-header-border-top row-widths))
          (if (list? (car header))
              (let lp ((r header)
                       (rnum 1))
                (when (find (lambda (c)
                              (not (null? c)))
                            r)
                  (let ((part (map (lambda (e)
                                     (cond
                                      ((null? e)
                                       "")
                                      (else
                                       (car e))))
                                   r)))
                    (table-format-row part
                                      #:cell-widths row-widths
                                      #:borders borders
                                      #:separator header-column-separator
                                      #:row-number rnum
                                      #:padding padding
                                      #:type 'header
                                      #:row-number rnum)
                    (lp (map (lambda (e)
                               (cond
                                ((null? e)
                                 '())
                                (else
                                 (cdr e))))
                             r)
                        (+ rnum 1)))))
              (table-format-row header
                                #:cell-widths row-widths
                                #:borders borders
                                #:separator header-column-separator
                                #:type 'header
                                #:padding padding
                                #:row-number 1))
          (when header-bottom
            (display-header-border-bottom row-widths))
          (display-table (cdr table)))
        (display-table table))

    (when (and shadow shadow-offset)
        (when (> shadow-y-offset 0)
          (let ((str (with-output-to-string
                       (lambda ()
                         (display-line row-widths
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
                              (display (stylize (string-copy str (abs shadow-x-offset))
                                                shadow-style)
                                       port))
                            (begin
                              (display (stylize (string* shadow (abs shadow-x-offset))
                                                shadow-style)
                                       port)
                              (display (stylize str shadow-style) port))))
                      (iota shadow-y-offset)))))))

;;; table.scm ends here.

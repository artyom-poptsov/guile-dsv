;;; dsv.scm -- DSV parser.

;; Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;;
;; Procedures for working with delimiter-separated values (DSV) format that is
;; widespread in the Unix world.  Notable examples of DSV are /etc/passwd and
;; /etc/inittab files.  Default delimiter is set to a colon.
;;
;; Some examples:
;;
;;   (dsv-string->list "a:b:c")
;;   => '("a" "b" "c")
;;
;;   (dsv-string->list "a;b;c" #\;)
;;   => '("a" "b" "c")
;;
;;   (dsv-string->list "a,b\\,c" #\,)
;;   => '("a" "b,c")
;;
;;   (list->dsv-string '("a" "b" "c"))
;;   => "a:b:c"
;;
;;   (dsv-read (open-input-file "/etc/passwd"))
;;   => (...
;;       ("news" "x" "9" "13" "news" "/usr/lib/news" "/bin/false")
;;       ("root" "x" "0" "0" "root" "/root" "/bin/zsh"))
;;
;;   (guess-delimiter "a:b,c,d")
;;   => #\,
;;
;; These procedures are exported:
;;
;;   dsv-string->list string [delimiter]
;;   list->dsv-string list [delimiter]
;;   dsv-read [port [delimiter]]
;;   dsv-write [port [delimiter]]
;;   guess-delimiter string
;;


;;; Code:

(define-module (dsv)
  ;; DSV
  #:use-module ((dsv rfc4180) #:renamer (symbol-prefix-proc 'rfc4180:))
  #:use-module ((dsv unix)    #:renamer (symbol-prefix-proc 'unix:))

  #:export (dsv-string->scm
            scm->dsv-string
            dsv->scm
            scm->dsv
            guess-delimiter))

(define* (dsv-string->scm str
                          #:optional (delimiter 'default)
                          #:key (format 'unix))
  "Convert a DSV string STR to a list of values using a DELIMITER.  If the
DELIMITER is not set, use the default delimiter (colon).  Return a list of
values."
  (case format
    ((unix)
     (unix:dsv-string->scm str (if (equal? delimiter 'default)
                                   unix:%default-delimiter
                                   delimiter)))
    ((rfc4180)
     (rfc4180:dsv-string->scm str (if (equal? delimiter 'default)
                                      rfc4180:%default-delimiter
                                      delimiter)))
    (else
     (error "Unknown format" format))))

(define* (scm->dsv-string lst
                           #:optional (delimiter 'default)
                           #:key (format 'unix))
  "Convert a list LST to a DSV string using a DELIMITER.  If the DELIMITER is
not set, use the default delimiter (colon).  Return a DSV string.

Example:

  (list->dsv-string '(\"a\" \"b\" \"c\"))
  => \"a:b:c\"
"
  (case format
    ((unix)
     (unix:scm->dsv-string lst (if (equal? delimiter 'default)
                                   unix:%default-delimiter
                                   delimiter)))
    ((rfc4180)
     (rfc4180:scm->dsv-string lst (if (equal? delimiter 'default)
                                   rfc4180:%default-delimiter
                                   delimiter)))
    (else
     (error "Unknown format" format))))


(define* (dsv->scm #:optional
                   (port      (current-input-port))
                   (delimiter 'default)
                   #:key
                   (format         'unix)
                   (comment-symbol #\#))
  "Read DSV data from a PORT.  If the PORT is not set, read from the default
input port.  If a DELIMITER is not set, use the default delimiter (colon).
Skip lines commented with a COMMENT-SYMBOL.  Return a list of values."

  (case format
    ((unix)
     (unix:dsv->scm port (if (equal? delimiter 'default)
                             unix:%default-delimiter
                             delimiter)
                    comment-symbol))
    ((rfc4180)
     (rfc4180:dsv->scm port (if (equal? delimiter 'default)
                             rfc4180:%default-delimiter
                             delimiter)
                       comment-symbol))
    (else
     (error "Unknown format" format))))

(define* (scm->dsv lst
                   #:optional
                   (port      (current-output-port))
                   (delimiter 'default)
                   #:key
                   (format    'unix))
  "Write a list of values LST as a sequence of DSV strings to a PORT.
If the PORT is not set, write to the default output port.  If a DELIMITER is
not set, use the default delimiter (colon).  FORMAT allows to specify a DSV
format style."
  (case format
    ((unix)
     (unix:scm->dsv lst port (if (equal? delimiter 'default)
                                 unix:%default-delimiter
                                 delimiter)))
    ((rfc4180)
     (rfc4180:scm->dsv lst port (if (equal? delimiter 'default)
                                 rfc4180:%default-delimiter
                                 delimiter)))
    (else
     (error "Unknown format" format))))


(define* (guess-delimiter str #:key (format 'unix))
  (case format
    ((unix)
     (unix:guess-delimiter str))
    ((rfc4180)
     ;; FIXME: Implement this.
     (error "Format is not supported yet." format))
    (else
     (error "Unknown format." format))))

;;; dsv.scm ends here.

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
;; widespread in the Unix world.  Notable examples of DSV are '/etc/passwd' and
;; '/etc/inittab' files.
;;
;; Most of the procedures take an optinal #:format argument that specifies the
;; format of DSV.  Default value of the #:format is 'unix'.
;;
;; Some examples:
;;
;;   (dsv-string->scm "a:b:c")
;;   => '(("a" "b" "c"))
;;
;;   (dsv-string->scm "a;b;c" #\;)
;;   => '(("a" "b" "c"))
;;
;;   (dsv-string->scm "a,b\\,c" #\,)
;;   => '(("a" "b,c"))
;;
;;   (scm->dsv-string '(("a" "b" "c")))
;;   => "a:b:c"
;;
;;   (dsv->scm (open-input-file "/etc/passwd"))
;;   => (...
;;       ("news" "x" "9" "13" "news" "/usr/lib/news" "/bin/false")
;;       ("root" "x" "0" "0" "root" "/root" "/bin/zsh"))
;;
;;   (guess-delimiter "a:b,c,d")
;;   => #\,
;;
;; These procedures are exported:
;;
;;   dsv-string->scm string [delimiter] [#:format 'unix]
;;   scm->dsv-string list [delimiter] [#:format 'unix]
;;   dsv->scm [port [delimiter]] [#:format 'unix] [#:comment-prefix #\#]
;;   scm->dsv [port [delimiter]] [#:format 'unix]
;;   guess-delimiter string [#:format 'unix]
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


(define* (dsv->scm #:optional
                   (port      (current-input-port))
                   (delimiter 'default)
                   #:key
                   (format         'unix)
                   (comment-prefix #\#))
  "Read DSV data from a PORT.  If the PORT is not set, read from the default
input port.  If a DELIMITER is not set, use the default delimiter for a
FORMAT.  Skip lines commented with a COMMENT-PREFIX.  Return a list of
values."

  (case format
    ((unix)
     (let ((parser (unix:make-parser port delimiter 'default
                                     comment-prefix)))
       (unix:scm->dsv parser)))
    ((rfc4180)
     (let ((parser (rfc4180:make-parser port delimiter 'default
                                        comment-prefix)))
       (rfc4180:dsv->scm parser)))
    (else
     (error "Unknown format" format))))

(define* (dsv-string->scm str
                          #:optional (delimiter 'default)
                          #:key
                          (format 'unix)
                          (comment-prefix 'default))
  "Convert a DSV string STR to a list of values using a DELIMITER.  If the
DELIMITER is not set, use the default delimiter for a FORMAT.  Skip lines
commented with a COMMENT-PREFIX.  Return a list of values."
  (case format
    ((unix)
     (let ((parser (unix:make-string-parser str delimiter 'default comment-prefix)))
       (unix:dsv->scm parser)))
    ((rfc4180)
     (let ((parser (rfc4180:make-string-parser str delimiter 'default comment-prefix)))
       (rfc4180:dsv->scm parser)))
    (else
     (error "Unknown format" format))))


(define* (scm->dsv lst
                   #:optional
                   (port      (current-output-port))
                   (delimiter 'default)
                   #:key
                   (format    'unix))
  "Write a list of values LST as a sequence of DSV strings to a PORT using a
specified DSV FORMAT.  If the PORT is not set, write to the default output
port.  If a DELIMITER is not set, use the default delimiter for a FORMAT."
  (let ((lst (if (or (null? lst) (list? (car lst)))
                 lst
                 (list lst))))
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
       (error "Unknown format" format)))))

(define* (scm->dsv-string lst
                           #:optional (delimiter 'default)
                           #:key (format 'unix))
  "Convert a list LST to a DSV string using a specified DSV FORMAT.  If the
DELIMITER is not set, use the default delimiter for a FORMAT.  Return a DSV
string."
  (let ((lst (if (or (null? lst) (list? (car lst)))
                 lst
                 (list lst))))
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
       (error "Unknown format" format)))))


(define* (guess-delimiter str #:optional (known-delimiters 'default)
                          #:key (format 'unix))
  "Guess a DSV string STR delimiter.  Optionally accept list of
KNOWN-DELIMITERS as an argument.  The procedure returns guessed delimiter or
'#f' if it cannot determine a delimiter based on the given arguments.

Note that when KNOWN-DELIMITERS list contains less than two elements, the
procedure returns '#f'."
  (case format
    ((unix)
     (let ((parser (unix:make-string-parser str 'default known-delimiters
                                            'default)))
       (unix:guess-delimiter parser)))
    ((rfc4180)
     (let ((parser (rfc4180:make-string-parser str 'default known-delimiters
                                               'default)))
       (rfc4180:guess-delimiter parser)))
    (else
     (error "Unknown format." format))))

;;; dsv.scm ends here.

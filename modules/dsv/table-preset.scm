;;; preset.scm -- Guile-DSV table presets.

;; Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

;; Table configuration presets.


;;; Code:


(define-module (dsv table-preset)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 regex)
  #:use-module (scheme documentation)
  #:use-module (dsv cli config)
  #:re-export  (%table-presets-path)
  #:export (print-table-presets
            load-table-preset
            preset-name?))


(define (print-table-presets port)
  "Print all known table presets to a PORT."
  (let ((dir (scandir %table-presets-path
                      (lambda (name)
                        (not (string-prefix? "." name))))))
    (for-each (lambda (d)
                (format port
                        "  ~a~%"
                        (car (string-split d #\.))))
              dir)))

(define (load-table-preset file)
  "Read table borders specification form a FILE.  Return the specification as a
+list."
  (let ((file-path (string-append %table-presets-path
                                  file
                                  ".scm")))
    (unless (file-exists? file-path)
      (format (current-error-port)
              "Could not find a preset \"~a\" (no such file: \"~a\")~%" file file-path)
      (format (current-error-port)
              "Known presets are:~%")
      (print-table-presets (current-error-port))
      (exit 1))
    (let ((p (open-input-file file-path)))
      (let ((result (read p)))
        (close p)
        result))))

(define (preset-name? spec)
  "Check if a SPEC is a table preset name."
  (string-match "^[a-zA-Z\\-]+$" spec))

;;; preset.scm ends here.

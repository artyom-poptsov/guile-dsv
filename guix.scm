;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2017 Alex Sassmannshausen <alex@pompo.co>
;;
;; Author: Alex Sassmannshausen <alex@pompo.co>
;; Created: 2 November 2017
;;
;; This file is part of Guile-DSV.
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
;; GNU Guix development package. To use as the basis for a development
;; environment, run:
;;
;;  guix environment --pure --container -l guix.scm
;;
;; In the new shell, run:
;;
;;  autoreconf -vif && ./configure && make check
;;
;;; Code:

(use-modules (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages pkg-config)
             (gnu packages texinfo))

(package
  (name "guile-dsv")
  (version "0.2.0")
  (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/a-sassmannshausen/guile-dsv")
                                        ;(url "https://github.com/artyom-poptsov/guile-dsv")
                  (commit "e66b60e24100ae79fc2660fe87ecf91d78e3e6c5")))
            (file-name (string-append name "-" version "-checkout"))
            (sha256
             (base32
              "0ywb0hdbs4lcjag8b3id43fpyn5s6gscg7dk0n9ryigyvch80wxj"))))
  (build-system gnu-build-system)
  (native-inputs
   `(("autoconf" ,autoconf)
     ("automake" ,automake)
     ("pkg-config" ,pkg-config)
     ("texinfo" ,texinfo)))
  (inputs `(("guile" ,guile-2.2)))
  (propagated-inputs `(("guile-lib" ,guile2.2-lib)))
  (arguments
   '(#:phases (modify-phases %standard-phases
                (add-after 'unpack 'autoreconf
                  (lambda _
                    (zero? (system* "autoreconf" "-vfi")))))))
  (home-page "https://github.com/artyom-poptsov/guile-dsv")
  (synopsis "DSV module for Guile")
  (description
   "Guile-DSV is a GNU Guile module for working with the
delimiter-separated values (DSV) data format.

Guile-DSV supports the Unix-style DSV format and RFC 4180 format.
")
  (license gpl3))

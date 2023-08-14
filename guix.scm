;; guix.scm --- GNU Guix package recipe    -*- coding: utf-8 -*-
;;
;; Copyright (C) 2017 Alex Sassmannshausen <alex@pompo.co>
;; Copyright (C) 2021-2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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


(use-modules (guix gexp)
             (guix packages)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (gnu packages autotools)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages pkg-config)
             (gnu packages texinfo)
             (gnu packages man)

             ;; For Guile-SMC
             (gnu packages bash)
             (gnu packages admin)
             (gnu packages base))


(define %source-dir (dirname (current-filename)))

(define-public guile-smc-0.6.2
  (package
    (name "guile-smc")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/artyom-poptsov/guile-smc")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version))
       (sha256
        (base32
         "11083lj048ab5zsdgwpkshxi8v5nfdr7kvmmslszbi7lq2pwfqig"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")     ;to prevent guild warnings
       #:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'strip)
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (bin       (string-append out "/bin"))
                    (guile-lib (assoc-ref inputs "guile-lib"))
                    (version   (target-guile-effective-version))
                    (scm       (string-append "/share/guile/site/"
                                              version))
                    (go        (string-append  "/lib/guile/"
                                               version "/site-ccache")))
               (wrap-program (string-append bin "/smc")
                 `("GUILE_LOAD_PATH" prefix
                   (,(string-append out scm)
                    ,(string-append guile-lib scm)))
                 `("GUILE_LOAD_COMPILED_PATH" prefix
                   (,(string-append out go)
                    ,(string-append guile-lib go))))))))))
    (native-inputs
     (list autoconf
           automake
           pkg-config
           texinfo
           help2man
           which
           ;; needed when cross-compiling.
           guile-3.0
           guile-lib))
    (inputs
     (list bash-minimal guile-3.0 guile-lib inetutils))
    (home-page "https://github.com/artyom-poptsov/guile-smc")
    (synopsis "GNU Guile state machine compiler")
    (description
     "Guile-SMC is a state machine compiler that allows users to describe
finite state machines (FSMs) in Scheme in terms of transition tables.  It is
capable to generate such transition tables from a @url{https://plantuml.com/,
PlantUML} state diagrams.

A transition table can be verified and checked for dead-ends and infinite
loops.  Also Guile-SMC FSMs gather statistics when they run.

Guile-SMC comes with a Scheme program called @command{smc} -- a state machine
compiler itself.  It produces a Scheme code for an FSM from the PlantUML
format.  This tool is meant to be called on a PlantUML file when a program
with a FSM is being built (for example, from a Makefile.)")
    (license gpl3)))


(package
  (name "guile-dsv")
  (version "git")
  (source (local-file %source-dir
                      #:recursive? #t
                      #:select? (git-predicate %source-dir)))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           pkg-config
           texinfo
           help2man
           ;; needed when cross-compiling.
           guile-3.0
           guile-lib
           guile-zlib
           guile-smc-0.6.2))
    (inputs (list guile-3.0))
    (propagated-inputs (list guile-lib guile-smc-0.6.2))
    (arguments
     `(#:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%gnu-build-system-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%gnu-build-system-modules)
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-program
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (guile-lib (assoc-ref inputs "guile-lib"))
                             (version (target-guile-effective-version))
                             (scm (string-append "/share/guile/site/"
                                                 version))
                             (go (string-append  "/lib/guile/"
                                                 version "/site-ccache")))
                        (wrap-program (string-append bin "/dsv")
                          `("GUILE_LOAD_PATH" prefix
                            (,(string-append out scm)
                             ,(string-append guile-lib scm)))
                          `("GUILE_LOAD_COMPILED_PATH" prefix
                            (,(string-append out go)
                             ,(string-append guile-lib go)))))
                      #t)))))
    (home-page "https://github.com/artyom-poptsov/guile-dsv")
    (synopsis "DSV module for Guile")
    (description
     "Guile-DSV is a GNU Guile module for working with the
delimiter-separated values (DSV) data format.  Guile-DSV supports the
Unix-style DSV format and RFC 4180 format.")
    (license gpl3+))

;;; guix.scm ends here.

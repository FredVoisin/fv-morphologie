;;;; This file is part of fv-morphologie.
;;;; Copyright (C) 1992-2026 Frederic Voisin
;;;;
;;;; fv-morphologie is derived from Morphologie
;;;; (https://github.com/openmusic-project/Morphologie), released under GPL-3.0.
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License version 3
;;;; as published by the Free Software Foundation.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;;
;;;; SPDX-License-Identifier: GPL-3.0-only

;;;; fv-morphologie.asd
;;;;
;;;; Common Lisp system definition for fv-morphologie.
;;;; (ql:quickload :fv-morphologie)  or  (asdf:load-system :fv-morphologie)
;;;; (asdf:test-system :fv-morphologie)
;;;;
;;;; FiveAM is used only by the test system (fv-morphologie/tests),
;;;; to test the code: fv-morphologie itself does not depend on it.

(asdf:defsystem :fv-morphologie
  :description "Lisp tools to analyse musical and syntagmatic sequences as symbolic expressions."
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :author "Frederic Voisin"
  :license "GPL-3.0-only"
  :version "1.1.1"
  :homepage "https://github.com/FredVoisin/fv-morphologie"
  :bug-tracker "https://github.com/FredVoisin/fv-morphologie/issues"
  :source-control (:git "https://github.com/FredVoisin/fv-morphologie.git")
  :depends-on (:uiop)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "encoding")
                             (:file "filters")
                             (:file "signature")
                             (:file "distances")
                             (:file "marks")
                             (:file "motifs")
                             (:file "graphs")
                             (:file "dot")
                             (:file "classification")
                             (:file "information")
                             (:file "interpolation")
                             (:file "io")
                             (:module "api"
                              :serial t
                              :components ((:file "transcription")
                                           (:file "classification")
                                           (:file "evaluation")
                                           (:file "files")
                                           (:file "help"))))))
  :in-order-to ((test-op (test-op :fv-morphologie/tests))))

(asdf:defsystem :fv-morphologie/tests
  :description "Unit tests for fv-morphologie (FiveAM is used for testing only)."
  :depends-on (:fv-morphologie :fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "encoding")
               (:file "filters")
               (:file "signature")
               (:file "distances")
               (:file "marks")
               (:file "motifs")
               (:file "graphs")
               (:file "dot")
               (:file "classification")
               (:file "information")
               (:file "io")
               (:file "help"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :fiveam :run!
                                       (uiop:find-symbol* :fv-morphologie :fv-morphologie/tests))
               (error "fv-morphologie: some tests failed."))))

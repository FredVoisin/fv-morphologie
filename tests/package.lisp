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

;;;; fv-morphologie tests
;;;; FiveAM is used here only for testing the code.
;;;; Run with (asdf:test-system :fv-morphologie)

(defpackage :fv-morphologie/tests
  (:use :cl :fiveam)
  (:import-from :fv-morphologie
                ;; encodage
                #:transcode #:alpha>num #:num>alpha #:num>base #:concaten #:list>sym
                #:str->symb #:split #:split-string
                #:filt-median #:filt-local-rep #:filt-lowpass #:filt-mean #:filt-fct #:filt-noise #:int-signature #:exsample
                ;; distances
                #:dist-euclid #:dist-euclidian #:dist-citybloc #:dist-hamming
                #:dist-edit #:dist-multi-edit #:dist-structure #:dist-graph
                ;; information
                #:histogram #:entropy #:elt-info
                ;; classification
                #:mark-position #:mark-list #:mark-structure #:motif-structure
                #:motif-find #:motif-list #:motif-group #:class-num #:class-sym
                ;; graphs
                #:graph-span #:graph-path #:graph-length #:graph-degree #:graph-nodes
                #:graph-extrem #:graph>dot #:graph-part
                ;; io
                #:read-text #:write-list #:date-string #:date+time-string))

(in-package :fv-morphologie/tests)

(def-suite fv-morphologie
  :description "All fv-morphologie tests.")

(defun seeded-random-state (&optional (seed 42))
  "A random state seeded with SEED, where the implementation allows it,
so that the tests of the random algorithms are reproducible."
  #+sbcl (sb-ext:seed-random-state seed)
  #+ecl (make-random-state seed)
  #-(or sbcl ecl) (make-random-state t))

(defun approx= (a b &optional (epsilon 1e-6))
  (< (abs (- a b)) epsilon))

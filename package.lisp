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

;;;; package.lisp
;;;;
;;;; The OMPW package (PWGL/OpenMusic bindings, see pwgl-legacy/) is used
;;;; only when it is already loaded, so the system also loads in a plain
;;;; LispWorks image.

(defpackage :fv-morphologie
  (:use :cl . #.(if (find-package :ompw) '(:ompw) '()))
  (:export
   ;; 1. transcription
   #:transcode #:num>base #:num>alpha #:alpha>num #:list>sym #:str->symb
   #:filt-noise #:filt-median #:filt-mean #:filt-lowpass #:filt-local-rep
   #:filt-fct
   #:graph-span #:graph-path #:graph>dot #:graph>matrix
   ;; 2. classification
   #:int-signature #:exsample #:split #:graph-part
   #:motif-group #:concaten
   #:mark-structure #:motif-structure #:class-num #:class-sym
   ;; 3. evaluation
   #:mark-position #:mark-list #:motif-find #:motif-list
   #:graph-nodes #:graph-extrem
   #:dist-euclidian #:dist-euclid #:dist-citybloc #:dist-hamming #:dist-edit
   #:dist-multi-edit #:dist-structure #:dist-graph
   #:histogram #:entropy #:elt-info #:inner-dynamic
   #:graph-length #:graph-degree
   ;; 4. read/write
   #:read-text #:write-list #:display-list
   #:split-string #:date-string #:date+time-string
   ;; documentation
   #:help #:doc))

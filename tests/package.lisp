;;;; fv-morphologie tests
;;;; FiveAM is used here only for testing the code.
;;;; Run with (asdf:test-system :fv-morphologie)

(defpackage :fv-morphologie/tests
  (:use :cl :fiveam)
  (:import-from :fv-morphologie
                ;; encodage
                #:transcode #:alpha>num #:num>alpha #:num>base #:concaten #:list>sym
                #:str->symb #:split #:split-string
                #:filt-median #:filt-local-rep
                ;; distances
                #:dist-euclid #:dist-euclidian #:dist-citybloc #:dist-hamming
                #:dist-edit #:dist-multi-edit #:dist-structure #:dist-graph
                ;; information
                #:histogram #:entropy #:elt-info
                ;; classification
                #:mark-position #:mark-list #:mark-structure
                #:motif-find #:motif-list #:motif-group #:class-num
                ;; graphs
                #:graph-span #:graph-path #:graph-length #:graph-degree
                #:graph-extrem #:graph>dot
                ;; io
                #:read-text #:write-list #:date-string #:date+time-string))

(in-package :fv-morphologie/tests)

(def-suite fv-morphologie
  :description "All fv-morphologie tests.")

(defun approx= (a b &optional (epsilon 1e-6))
  (< (abs (- a b)) epsilon))

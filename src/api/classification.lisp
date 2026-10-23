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

;;;; api/classification.lisp
;;;;
;;;; Public functions: differentiation (signatures, sampling, marks, motifs,
;;;; classification) and graph partition.

(in-package :fv-morphologie)

(defgeneric exsample (seq method mode thres &optional option)
  (:documentation "EXSAMPLE: samples a sequence of numerical values according to its signature (with its first and last values).
Available methods are (see int-signature for documentation):
- :minmax
- :minflexmax (default, or nil)
- :major-extrema
- :landmarks
Modes may be :values (default, or nil), :positions, :all or :resampled"))

(defmethod exsample ((seq list) (method symbol) (mode symbol) (thres t) &optional (option nil))
  ;; thres nil: no threshold, as 0 (no method applied to nil)
  (if seq
      (echantillonage seq (or method :minflexmax) (or mode :values) (or thres 0) option)
      nil))

(defgeneric split (seq &optional marks)
  (:documentation "SPLIT: splits <seq> according to marks, which are not kept, and returns the list of its segments.
A string is split at the characters of the string <marks> and at blanks.
A list is split at the elements of the list <marks> (compared with equalp), at a single mark, or where the predicate <marks> is true."))

(defmethod split ((seq list) &optional (marks nil))
  (split-list-using-marks seq marks))

(defmethod split ((seq string) &optional (marks " "))
  (split-string seq marks))

(defgeneric int-signature (seq mode thres &optional option)
  (:documentation "INT-SIGNATURE: signature of a sequence with some threshold <thres> for variation in intensity.
The argument <thres> may be nil or 0 (no threshold for variations in intensity), or an absolute amplitude (in the unit of the values of <seq>): variations smaller than <thres> are ignored.
 Argument <mode> define the algorithm to compute signature which may require the optional argument <option> :
:minflexmax, returns minima, maxima and inflexions: the minima and maxima of :minmax, and an inflexion where a plateau
 which is not an extremum begins and where it ends (a plateau is a stretch where the sequence varies by less than thres);
:minmax, minima and maxima only: the important extrema of :major-extrema, with |a - b|;
:major-extrema, important extrema (Fink & Gandhi, Conference on Systems, Man and Cybernetics, 2007):
 a minimum (maximum) such that the sequence rises (falls) by at least thres on both sides;
:landmarks, first-order landmarks smoothed by MDPP(D, P) (Perng, Parker & Leung, Conference on Information
 and Knowledge Management, 1999): pairs of close landmarks whose difference is below thres are removed.
Argument <thres> is the treshold value (delta) in amplitude.
Argument <option> is an optional parameter depending to the choosen method :
with :minflexmax, <option> is the minimal duration of a plateau, a number of positions (nil, 0, 1 and 2 mean 2);
with :minmax, <option> is not used;
with :major-extrema, <option> may be a distance function (a b) for comparing values (|a - b| by default);
with :landmarks, <option> is nil, a number D (only landmarks closer than D ranks may be removed), a function (a b) giving the
difference between two values (|a - b| by default), or a list (D criterion), criterion being :absolute, :relative
(2 |a - b| / (|a| + |b|), as in Perng et al.) or a function."))

(defmethod int-signature ((seq t) (mode t) (thres t) &optional (option nil))
  ;; thres nil: no threshold, as 0 (no method applied to nil)
  (when (not mode) (setf mode :minflexmax))
  (signature seq mode (or thres 0) option))

(defgeneric mark-structure  (seq out &key diss rem-loc-dup test)
  (:documentation "MARK-STRUCTURE: returns possible structures of input <seq> based on marks retrieved from <seq>.
The output is sorted by structures's length.
Use output menu <out> to define result form (another output is an error):
:struct (or nil) lists all possible structures according to different marks of <seq>
:pos each structure is consed to the list of positions for each segment followed by the list of the segments themselves
:raw each structure is consed to the list of the different segments, each followed by all its positions in <seq>.
Option :diss defines the dissemblance threshold to consider different segment as the same.
Option :test (equalp by default) defines the identity of elements: of the marks, of the elements of the segments
compared by the edit distance, and of the consecutive elements removed by :rem-loc-dup.
Option :rem-loc-dup (t by default) removes the local repetitions inside each segment; positions refer to <seq>."))

(defmethod mark-structure  ((seq t) (out t) &key (diss 0) (rem-loc-dup t) (test #'equalp))
  (when (not out) (setf out :struct))
  (mark-strct seq out diss rem-loc-dup test))

(defgeneric motif-structure (seq &key lengths test diss l-var criteria distance)
  (:documentation "MOTIF-STRUCTURE: structure of <seq> in terms of its repeated motifs, as a list of two elements:
the positions where motifs begin, each followed by the numbers of the motifs beginning there, then the motifs.
A motif is a segment repeated at least once, later and without overlapping.
:lengths the lengths of the motifs searched: (min max), or a maximum k for lengths 2 to k (default: 3 to half the length of <seq>);
:test the identity of elements (equalp by default);
:diss the tolerance, the maximum normalized edit distance (0 to 1) between a motif and its repetitions (0 by default: exact repetitions);
:l-var the length variation of the repetitions, a fraction of the length of the motif (0 by default; used with a tolerance);
:criteria :length (default) or :freq, to keep the longest or the most frequent of the motifs contained in one another;
:distance a function (a b) giving the distance between two segments (lists), instead of the edit distance; :diss is then in its unit."))

(defmethod motif-structure ((seq t) &key (lengths nil) (test #'equalp) (diss 0) (l-var 0) (criteria :length) (distance nil))
  (unless (member criteria '(:length :freq))
    (error "motif-structure: :criteria must be :length or :freq, not ~S." criteria))
  (unless (or (null lengths) (and (integerp lengths) (plusp lengths))
              (and (listp lengths) (= 2 (length lengths)) (every #'integerp lengths)
                   (<= 1 (first lengths) (second lengths))))
    (error "motif-structure: :lengths must be (min max) or a maximum length, not ~S." lengths))
  (fv-morphologie::rep-strct seq :diss diss :l-var l-var :n lengths :criteria criteria
                                 :distance-fct distance :test test))

(defgeneric motif-group (seq test &optional key)
  (:documentation "MOTIF-GROUP: assembles consecutive elements or motifs of <seq> for which the comparison <test> (by default: equalp) is successful.
Optional argument <key> defines a function to be applyed to each consecutive element or motif to be compared."))

(defmethod motif-group ((seq t) (test function) &optional (key nil))
  (adj-group seq test key))

(defgeneric class-num (data classes mode  &key iter dist)
  (:documentation "CLASS-NUM: classify a set (list) of points <data> into different <classes> into s space with n dimensions (n integer > 0),
where each point is represented as a list of values into each dimension.
Argument :mode defines the algorithm used for partitioning the points:
:centroids for centroids algorithm (in French: nuees dynamiques);
:1d-centroids, initial centres taken between the largest differences of the sorted data (one dimension, deterministic).
Keyword argument :iter to set the number of runs of the algorithm (with :centroids);
:dist to set the distance or metric to be used (by default: euclidian distance)."))

(defmethod class-num ((data t) (classes integer) (mode symbol) &key (iter nil) (dist nil))
  (setf classes (max 2 classes))   ; at least 2 classes
  ;; modes recognized by name (:centroids, 'centroids from any package); an unknown mode was nil
  (let ((name (if mode (string-upcase (string mode)) "CENTROIDS")))
    (cond ((string= name "CENTROIDS")
           (let ((numbers (every #'numberp data)))
             (unless (or numbers
                         (and (every #'listp data)
                              (or (null data) (apply #'= (mapcar #'length data)))))
               (error "class-num: the points of data must all be numbers, or lists of the same number of coordinates."))
             (if (and numbers data (null iter))
                 ;; the centres of numbers are numbers (they were (x 0), the points used internally)
                 (let ((r (fv-morphologie::n-class data classes iter dist)))
                   (list (first r) (mapcar #'car (second r)) (third r)))
                 ;; all the values (with an integer iter: the partition and its frequency)
                 (fv-morphologie::n-class data classes iter dist))))
          ((string= name "1D-CENTROIDS")
           (fv-morphologie::1d-class data classes (if dist dist #'identity)))
          (t (error "class-num: unknown mode ~S, use :centroids (or nil) or :1d-centroids." mode)))))

(defgeneric class-sym (data classes mode &key uncom ins del change excluded mst)
  (:documentation "CLASS-SYM: classify a set of segments or sequences into different classes according to their editing distance to each other and
by partitioning the resulting spanning tree (see class-graph).
The output is a list of numbers (from zero) as instances of the different classes retrieved.
Keyword arguments :uncom :ins/del :change for tuning editing distance (see dist-edit).
The argument :excluded is a list of elements of data excluded from classification ;
in the output, the numbering of the different instances begins by numbering these elements first (from zero), 
in the order defined with the argument :excluded."))

(defmethod class-sym ((data t) (classes integer) (mode symbol)
		      &key (uncom .5) (ins 1) (del 1) (change 1) (excluded nil) (mst nil))
  (setf classes (max 2 classes))
  (if (not excluded)
      (cond ((or (eq mode :edit-nn) (not mode))
             (fv-morphologie::s-class data classes nil change ins del uncom mst))
            ((eq mode :edit-norm)
             (fv-morphologie::s-class data classes t change ins del uncom mst))
            (t nil))
    (cond ((eq mode :edit-nn)
           (fv-morphologie::s-class-with-fixed data classes excluded nil nil nil change ins del uncom mst))
          ((eq mode :edit-norm)
           (fv-morphologie::s-class-with-fixed data classes excluded nil nil t change ins del uncom mst))
          (t nil))))

;; 2.5 CONCATENATION

;(defmethod concaten ((list nil))
;  :non-generic t
;  :class morphologie-box
;  (fv-morphologie::concaten list))

;(defmethod adj-group ((seq nil) (test #'eq) &optional (key nil))
;  :non-generic t
;  :class morphologie-box
;  (fv-morphologie::adj-group seq test key))

(defgeneric graph-part (graph &optional mode n)
  (:documentation  "GRAPH-PART: partition a graph according to edges which have maximum length.
Without <n>, all the edges of maximum length are cut ; with <n>, the n-1 longest edges are cut, giving n parts."))

(defmethod graph-part ((graph list) &optional (mode :distance) (n nil))
  (part-graph graph mode n))

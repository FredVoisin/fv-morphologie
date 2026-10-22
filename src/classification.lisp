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

;;;; classification.lisp
;;;;
;;;; Classification of numbers (moving centres, k-means, one dimension) and of
;;;; symbols (edit distances).

(in-package :fv-morphologie)

;;; CLASSIFICATION

(defun pos (item list)
  (loop for n from 0 to (1- (length list))
     when (= (nth n list) item) collect n))

(defun normalize-class (classes &optional (test #'equalp))
  "Reordonne les classes de nueesdynamiques, l'ordre de la classe etant le numero de sa premiere occurence dans la liste des classes."
  (let ((set nil) (r nil) (marker -1))
    (when (stringp classes) (setf classes (str->symb classes)))
    ;; set: the classes already seen, as (class . number), most recent first
    (dolist (c classes (nreverse r))
      (let ((seen (assoc c set :test test)))
        (if seen
            (push (cdr seen) r)
            (progn (push (cons c (incf marker)) set)
                   (push marker r)))))))

(defun norm-class (classes)
  "Reordonne les classes de class-1.
L'ordre de la classe etant le numero de sa premiere occurence dans la liste des classes.
Se connecte typiquement apres class-1 ou meta-class1.
IN : string or list (of symbols or lists or strings)."
  (if (or (stringp classes) (not (listp (car classes))))
      (normalize-class classes)
      (mapcar #'(lambda (x) (normalize-class x)) classes)))

(defun random-dot (data)
  "A random point in the space defined by data, a list of points (lists of coordinates): each coordinate
is drawn uniformly between the minimum and the maximum of this coordinate in data."
  ;; j below the number of coordinates (to: one too many, nth gave nil); always min + random
  ;; (it drew below the minimum when the maximum was negative); a range of 0 gives the value
  (loop for j below (length (car data))
        collect (let* ((values (mapcar #'(lambda (x) (nth j x)) data))
                       (min (reduce #'min values))
                       (range (- (reduce #'max values) min)))
                  (float (if (plusp range) (+ min (random (float range))) min)))))

;(random-dot '((0 0) (1 0) (0 -1) (2 -.1) (.1 -2)))

(defun isobarycentre (points)
  (let ((c (make-list (length (car points)) :initial-element 0))
        (l (length  points)))
    (loop until (null points)
       do (loop for p in (pop points) for i from 0 
	     do (incf (nth i c) p)))
    (mapcar #'(lambda (x) (float (/ x l))) c)))

;;; The 'Dynamic Clouds' algorithm (nuees dynamiques) starts from n distinct
;;; points of data chosen at random as centres. A class which becomes empty
;;; receives the point farthest from its centre, so that n classes are found
;;; as long as data has at least n distinct points.

(defgeneric nuees (data n &optional dist)
  (:documentation   "'Nuees dynamiques' algorithm :
Classify data into n classes (n integer), or around the initial centres n (n list of points).
Returns: classes of data, respective centers coordinates and iterations.
There are fewer than n classes only if data has fewer than n distinct points."))

(defun random-centres (data n)
  "n distinct points of data chosen at random (all of them if there are fewer)."
  (let ((points (remove-duplicates data :test #'equalp)))
    (loop repeat (min n (length points))
          collect (let ((p (nth (random (length points)) points)))
                    (setf points (remove p points :test #'eq))
                    (copy-list p)))))

(defmethod nuees ((data list) (n integer) &optional (dist #'dist-euclid))
  (nuees data (random-centres data n) dist))

;(nuees '((0 0) (.1 -.1) (.1 0)  (1 1) (1.1 1.1)  (.9 1) (1.9 .5)  (-2 -2) (-2.1 -1.5)) 3)
;(nuees '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 3)

(defun fill-empty-classes (r data centres dist)
  "Gives each empty class of r the point farthest from its centre, taken from a class with several points."
  (dotimes (k (length centres) r)
    (unless (member k r)
      (let ((far nil) (dmax -1))
        (loop for i from 0 for c in r for p in data
              do (when (> (count c r) 1)
                   (let ((d (funcall dist (nth c centres) p)))
                     (when (> d dmax) (setf dmax d far i)))))
        (when far
          (setf (nth far r) k
                (nth k centres) (copy-list (nth far data))))))))

(defmethod nuees ((data list) (n list) &optional (dist #'dist-euclid))
  (let* ((l (length data))
         (centres (copy-tree n))
         (r (make-list l))
         (fill (<= (length centres) (length (remove-duplicates data :test #'equalp))))
         (iter 0)
         (nc (make-list (length centres)))
         (z 0)
         tmp nr)
    (when (not dist) (setf dist #'dist-euclid))
    (loop until (or (if tmp (equalp (car tmp) (cadr tmp)) nil)
                    (>= iter 1000))
          do (incf iter)
             (setf tmp (list (copy-tree r) (car tmp)))
             (loop for i from 0 to (1- l) do ; attribution des classes selon centre
               (let ((d (mapcar #'(lambda (c) (when c (funcall dist c (nth i data)))) centres)))
                 (let ((m (apply #'min (remove nil d))))
                   (setf (nth i r) (position-if #'(lambda (x) (and x (= x m))) d)))))
             (when fill (fill-empty-classes r data centres dist))
             (dolist (i (remove-duplicates r))  ; mise a jour des centres de gravite de chaque classe
               (let ((elts (loop for j from 0 to (1- l)
                                 when (= i (nth j r))
                                   collect (nth j data))))
                 (when elts (setf (nth i centres) (isobarycentre elts))))))
    (setf nr (normalize-class r #'=)) ; output
    (loop until (or (not (member nil nc)) (= z l)) do
      (setf (nth (nth z nr) nc) (nth (nth z r) centres))
      (incf z))
    (values nr nc iter)))

(defgeneric dynclouds (data n &optional iter dist)
  (:documentation "Classification of data into N classes using the 'dynamic clouds' algorithm.
Adapted from Diday & all: Elements d'analyse de donnees, Dunod, Paris, 1982.
The first optional argument <iter> fixes the output form:
if nil, runs the algorithm once and returns (classes centres iterations) ;
if <iter> is a list of one number k, runs the algorithm k times and returns each different classification
with its frequency (the frequencies sum to 1), the most frequent first ;
if <iter> is a number k, runs the algorithm k times and returns two values: the most frequent classification and its frequency ;
if t, the same with N * N runs, where N is the number of classes to be found, and returns the most frequent classification.
The second argument <dist> is the distance function (#'dist-euclid by default).
"))

(defmethod dynclouds ((data list) (n t) &optional (iter nil) (dist nil))
  (when (not (member nil (mapcar #'numberp data)))
    (setf data (mat-transpose (list data (make-list (length data) :initial-element 0)))))
  (cond
   ((null iter) (multiple-value-bind (r c i) (nuees data n dist) (list r c i)))
   ((listp iter) (let* ((k (max 1 (car iter)))
                        (r (loop repeat k
                                 collect (values (nuees data n dist))))
                        (p (remove-duplicates r :test #'equalp)))
                   (sort (mapcar #'(lambda (x)
                                     (list x (float (/ (count x r :test #'equalp) k))))
                                 p)
                         '> :key #'cadr)))
   ((integerp iter)
    (let ((r (dynclouds data n (list iter) dist)))
      (values (caar r) (cadar r))))
   (t (caar (dynclouds data n (list (* n n)) dist)))))

;(dynclouds '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 2 nil)
;(dynclouds '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 2 t)
;(dynclouds '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 2 8)
;(dynclouds '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 2 '(8))

(defgeneric n-class (data n &optional iter dist)
  (:documentation "Classification of data into N classes using the 'dynamic clouds' algorithm.
Adapted from Diday & all: Elements d'analyse de donnees, Dunod, Paris, 1982.
The first optional argument <iter> fixes the output form:
if nil, runs the algorithm once and returns (classes centres iterations) ;
if <iter> is a list of one number k, runs the algorithm k times and returns each different classification
with its frequency (the frequencies sum to 1), the most frequent first ;
if <iter> is a number k, runs the algorithm k times and returns two values: the most frequent classification and its frequency ;
if t, the same with N * N runs, where N is the number of classes to be found, and returns the most frequent classification.
The second argument <dist> is the distance function (#'dist-euclid by default).
"))

(defmethod n-class ((data list) (n t) &optional (iter nil) (dist nil))
  (if data (dynclouds data n iter dist) nil))

(defgeneric 1d-class (data n &optional fct)
  (:documentation "quantifies data by partitioning dx."))

(defun 1d-cut-centres (sx cuts)
  "Means of the segments of the sorted list sx, cut after each position of cuts."
  (let ((start 0))
    (loop for end in (append (sort (copy-list cuts) '<) (list (1- (length sx))))
          collect (isobarycentre (mapcar #'list (subseq sx start (1+ end))))
          do (setf start (1+ end)))))

(defun 1d-nuees (data centres)
  "Runs the dynamic clouds algorithm on 1D data from the initial centres."
  (multiple-value-bind (r c i)
      (nuees (mat-transpose (list data (make-list (length data) :initial-element 0)))
             (mapcar #'(lambda (x) (list (car x) 0)) centres))
    (list r (mapcar #'car c) i)))

(defmethod 1d-class ((data list) (n null) &optional (fct #'identity))
  (declare (ignore n))
  (let* ((sx (sort (copy-list data) '<))
         (fx (mapcar fct (dx sx)))
         (c2 (nuees (mat-transpose (list fx (make-list (length fx) :initial-element 0))) 2))
         ;; the class of the largest gaps
         (big (nth (position (apply #'max fx) fx) c2)))
    (1d-nuees data (1d-cut-centres sx (allpos big c2)))))

(defmethod 1d-class ((data list) (n integer) &optional (fct #'identity))
  (let* ((sx (sort (copy-list data) '<))
         (fx (mapcar fct (dx sx)))
         (k (min (1- n) (count-if #'plusp fx)))
         ;; positions of the n-1 largest gaps
         (cuts (mapcar #'cdr (subseq (stable-sort (loop for g in fx for i from 0 collect (cons g i))
                                                  '> :key #'car)
                                     0 k))))
    (1d-nuees data (1d-cut-centres sx cuts))))

;;;
;;; classification par partitionnement de graphe

(defun sym-isocentre (data)
  "To make simple, the centre of a list of words is the list of words with no duplicates ;)"
  (remove-duplicates (mapcar #'append data) :test #'equalp))

(defun s-class (data n &optional (norm nil) (change 1) (insert 1) (delete 1) (inex 1) (mst nil)) ; (dist #'e-dist 1 1 .5)
  "Classification of symbolic data into n classes according to their ressemblance:
the n-1 longest edges of the minimum spanning tree of the data are cut (single linkage).
Returns the class of each datum, numbered by order of first appearance, and the list of classes."
  (let* ((symlist (remove-duplicates data :test #'equalp))
         (grm (cond (mst mst)
                    ((cdr symlist)
                     (minimum-spanning-tree (edit-dist symlist nil change insert delete inex norm)))
                    (t nil)))
         ;; the edges kept: all but the n-1 longest (the first one in the tree if equal)
         (kept (nthcdr (min (max 0 (1- n)) (length grm))
                       (stable-sort (copy-list grm) '> :key #'third)))
         (symclass (tree-components (length symlist) kept))
         (class-of (mapcar #'(lambda (d) (nth (position d symlist :test #'equalp) symclass)) data))
         (renum (normalize-class class-of #'=)))
    (values renum
            (loop for c from 0 to (if renum (apply #'max renum) -1)
                  collect (list c (loop for s in symlist for i from 0
                                        when (= c (nth (position s data :test #'equalp) renum))
                                          collect (list i s)))))))

(defun s-class-with-fixed (data n fixed &optional (fixed-dist nil) (norm nil) (mode t) (change 1) (insert 1) (delete 1) (inex 1) (mst nil))
  (declare (ignore mode))
  (let* ((f-dist (if fixed-dist fixed-dist #'equalp)) ; may be edit-dist change insert inex ?
         (f-length (length fixed))
         (fixed-pos (mat-transpose (list (mapcar #'(lambda (f) (allpos f data f-dist)) fixed) fixed)))
         (data-f (remove-if #'(lambda (x) (member x fixed :test f-dist)) data)) ; :test #'equalp))
         (c-data (s-class data-f n norm change insert delete inex mst)))
    (loop ;for d in data
          for i from 0 to (1- (length data))
          collect (let ((k (assoc i fixed-pos :test #'member)))
		    (if k
			(position (cadr k) fixed :test #'equalp)
			(+ f-length (pop c-data)))))))

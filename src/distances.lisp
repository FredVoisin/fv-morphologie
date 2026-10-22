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

;;;; distances.lisp
;;;;
;;;; Distances and similarities: euclidian, city-block, Hamming, edit
;;;; distances, Jaccard, Sorensen, Mountford, structures.

(in-package :fv-morphologie)

;;; DISTANCES DISSEMBLANCE ETC

;; 1. Distance euclidienne

(defgeneric dist-euclid (a b &optional key)
  (:documentation "Euclidian distance between points a and b.
Point coordinates are represented by a list of values in any euclidian space.
 if a is a list of coordinates (lists) and b null,
output is the list of distances for all points to all others ;
 if a or b is a number, consider b or a as list of numbers (1 dim).
If a and b are lists with not same size (i.e points with not same dimensions),
 returns NIL (no distance)."))

(defmethod dist-euclid ((a number) (b number) &optional (key #'identity))
  (declare (ignore key))
  (abs (- b a)))

(defmethod dist-euclid ((a list) (b number) &optional (key #'identity))
  ;(assert (not (member 'nil (mapcar #'numberp a))))
  (mapcar #'(lambda (x) (dist-euclid x b key)) a))

(defmethod dist-euclid ((a number) (b list) &optional (key #'identity))
  ;(assert (not (member 'nil (mapcar #'numberp b))))
  (mapcar #'(lambda (x) (dist-euclid a x key)) b))

(defmethod dist-euclid ((a list) (b list) &optional (key #'identity))
  ;(assert (equalp (mapcar #'numberp a) (mapcar #'numberp b)))
  (if key
      (sqrt (apply #'+ (loop for i in (funcall key a) for j in (funcall key b) collect (expt (- j i) 2))))
    (sqrt (apply #'+ (loop for i in a for j in b collect (expt (- j i) 2))))
    ))

(defmethod dist-euclid ((a list) (b null) &optional (key #'identity))
  ;(assert (apply #'= (mapcar #'length a)))
  (let ((r (list)))
    (dotimes (i (length a) (nreverse r))
      (loop for j from (1+ i) to (1- (length a))
            do (push (list i j (dist-euclid (nth i a) (nth j a) key)) r)))))

(defgeneric city-bloc (a b)
  (:documentation "City-bloc distance between a and b in euclidian space with n dimensions (with integer n > 0)."))

(defmethod city-bloc ((a number) (b number))
 (if (> a b) (- a b) (- b a)))

(defmethod city-bloc ((a list) (b list))
  (assert (= (length a) (length b)))
  (apply #'+ (mapcar #'(lambda (x y) (city-bloc x y)) a b)))

(defmethod city-bloc ((a list) (b null))
  (assert (apply #'= (mapcar #'length a)))
  (let ((r (list)))
    (dotimes (i (length a) (nreverse (remove 'nil r)))
      (loop for j from (1+ i) to (1- (length a)) do
	 (push (list i j (city-bloc (nth i a) (nth j a))) r)))))

(defgeneric close-p (a b thresh &optional dist)
  (:documentation "Tests if a and b are close according to their distance."))

(defmethod close-p ((a list) (b list) (thresh number) &optional (dist #'dist-euclid))
  (let ((d (funcall dist a b)))
    (when d (<= d thresh))))

(defmethod close-p ((a number) (b number) (thresh number) &optional (dist #'dist-euclid))
  (let ((d (funcall dist a b)))
    (when d (<= d thresh))))

(defmethod close-p ((a null) (b null) (thresh number) &optional (dist #'dist-euclid))
  (declare (ignore dist))
  nil)

(defmethod close-p ((a t) (b null) (thresh number) &optional (dist #'dist-euclid))
  (declare (ignore dist))
  nil)

(defmethod close-p ((a null) (b t) (thresh number) &optional (dist #'dist-euclid))
  (declare (ignore dist))
  nil)

; (close-p '(1.1 1.9) '(1 2) .5)


(defun hamming-distance (v1 v2 &optional (test #'eq))
  (assert (= (length v1) (length v2)))
  (let ((d 0))
    (dotimes (i (length v1) d)
      (when (not (funcall test (nth i v1) (nth i v2)))
        (incf d)))))

(defgeneric hamming-dist (v1 v2 &optional norm test)
  (:documentation "Hamming distance."))

(defmethod hamming-dist ((v1 list) (v2 list) &optional (norm t) (test #'eq))
  (let ((d (hamming-distance v1 v2 test)))
    (if norm (float (/ d (length v1))) d)))

(defmethod hamming-dist ((v1 list) (v2 null) &optional (norm t) (test #'eq))
  (let ((r (list)))
    (dotimes (i (1- (length v1)) (nreverse r))
      (dotimes (j (- (length v1) i 1))
        (push (list i (+ 1 j i) (hamming-dist (nth i v1) (nth (+ 1 j i) v1) norm test)) r)))))

;; 2. 'Editing distance' (Distance d'edition)
;; I have red a paper on genomic analysis on ADN using, as I remember, also used by a unix text commande.
;; Finally, as I understand it now, it's a kind of measure of the work of a Turing Machine:
;; the less it does, the closest it is...
;; (I may write here a Turing machine here to do the same :)

(defun edit-dist1 (seq1 seq2 &key (change 1) (insert 1) (delete 1) (normalized nil) (test #'equalp))
"Smallest distance between two lists of symbols.
 Keywords arguments:
  change: cost if changing a elt in sequences;
  insert: cost if deleting or inserting;
  normalized (T or NIL): normalized distance - 0 the closest, 1 the farest."
  (let* ((a (coerce seq1 'vector))
         (b (coerce seq2 'vector))
         (la (length a))
         (lb (length b))
         (prev (make-array (1+ la)))
         (row (make-array (1+ la))))
    ;; row j of the matrix of costs: (aref row i) for the first i elements of seq1, j of seq2
    (dotimes (i (1+ la)) (setf (aref prev i) (* i delete)))
    (loop for j from 1 to lb
          do (setf (aref row 0) (+ (aref prev 0) insert))
             (loop for i from 1 to la
                   do (setf (aref row i)
                            (min (+ (aref prev (1- i))
                                    (if (funcall test (aref a (1- i)) (aref b (1- j))) 0 change))
                                 (+ (aref prev i) insert)
                                 (+ (aref row (1- i)) delete))))
             (rotatef prev row))
    (let ((d (aref prev la)))
      (if normalized
          (if (zerop (max la lb)) 0.0 (float (/ d (max la lb))))
          d))))

(defun edit-dist2 (seq1 seq2 &key (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  "Returns the smallest distance between two lists of symbols.
 Keywords arguments:
  change: cost if changing a elt in sequences;
  insert: cost if deleting or inserting;
  inex = cost if changing with an element which doesn't exist on the other sequence;
  normalized (T or NIL): normalized distance, 0 the closest, 1 the farest."
  (let* ((a (coerce seq1 'vector))
         (b (coerce seq2 'vector))
         (la (length a))
         (lb (length b))
         ;; 1 for an element which does not exist (according to test) in the other sequence
         (ex (map 'vector #'(lambda (x) (if (member x seq2 :test test) 0 1)) a))
         (ey (map 'vector #'(lambda (y) (if (member y seq1 :test test) 0 1)) b))
         (prev (make-array (1+ la)))
         (row (make-array (1+ la))))
    (setf (aref prev 0) 0)
    (loop for i from 1 to la
          do (setf (aref prev i) (+ (aref prev (1- i)) delete (* (aref ex (1- i)) inex))))
    (loop for j from 1 to lb
          do (setf (aref row 0) (+ (aref prev 0) insert (* (aref ey (1- j)) inex)))
             (loop for i from 1 to la
                   do (let ((exi (aref ex (1- i))) (eyj (aref ey (1- j))))
                        (setf (aref row i)
                              (min (+ (aref prev (1- i))
                                      (if (funcall test (aref a (1- i)) (aref b (1- j)))
                                          0
                                          (+ change (* (+ exi eyj) inex))))
                                   (+ (aref prev i) insert (* eyj inex))
                                   (+ (aref row (1- i)) delete (* exi inex))))))
             (rotatef prev row))
    (let ((d (aref prev la)))
      (if normalized
          (if (zerop (max la lb)) 0.0 (float (/ d (max la lb))))
          d))))

(defun editing-dist (seq1 seq2 change insert delete inex normalized test)
"Editing distance between sequences of symbols.
 seq1: first sequence;
 seq2: second sequence;
 change: cost for changing a symbol;
 insert: cost for deleting or inserting a symbol;
 inex: cost for changig with a symbol from seq2 which doesn't exist in seq1;
 normalized: normalized output, in ratio to the length of the longest sequence;
 test: any binary function to compare symbols (default: #'equalp)."
 (if (not inex)
     (edit-dist1 seq1 seq2 :change change :insert insert :delete delete :normalized normalized :test test)
     (if (zerop inex)
	 (edit-dist1 seq1 seq2 :change change :insert insert :delete delete :normalized normalized :test test)
	 (edit-dist2 seq1 seq2 :change change :insert insert :delete delete :inex inex :normalized normalized :test test))))

(defgeneric edit-dist (seq1 seq2 &optional change insert delete inex normalized test)
  (:documentation
"Dissemblance measurement between sequences of symbols <seq1> and <seq2>, according to the 'editing distance';
 the dissemblance is nul (zero) when the sequences are the considered to be the same or 'equal'.
 Argument <change> is the cost value when changing a symbol ;
 Argument <insert> is the cost value when inserting a symbol ;
 Argument <delete> is the cost value when deleting a symbol ;
 Argument <inex> is the cost value when changing a symbol in one of the sequence which does not exist in the other ;
 Argument <normalized> to choose (un-)normalized output. If normalized, value 1.0 in ratio to the length of the longest sequence to be compared;
 test: any binary function to compare symbols (default: #'equalp)."))

(defmethod edit-dist ((seq1 list) (seq2 list) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  ;(if (not (member t (mapcar #'listp seq2)))
  ;    (mapcar #'(lambda (a b) (edit-dist 
  (editing-dist seq1 seq2 change insert delete inex normalized test))

(defmethod edit-dist ((seq1 list) (seq2 null) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (declare (ignore seq2))
  (when (not (= (length seq1)
		(length (remove-duplicates (copy-tree seq1) :test #'equalp))))
    (message "gen-dist: warning ! duplicates in input list."))
  (let ((r nil) (l (length seq1)))
    (dotimes (i l (nreverse r))
      (dotimes (j (- l i 1))
        (push (list i (+ i j 1) (edit-dist (nth i seq1) (nth (+ i j 1) seq1) change insert delete inex normalized test)) r))) ))

(defmethod edit-dist ((seq1 string) (seq2 string) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (string2code seq1) (string2code seq2) change insert delete inex normalized test))

(defmethod edit-dist ((seq1 symbol) (seq2 symbol) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (alpha>num seq1) (alpha>num seq2) change insert delete inex normalized test))

(defmethod edit-dist ((seq1 symbol) (seq2 number) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (alpha>num seq1) (alpha>num seq2) change insert delete inex normalized test))

(defmethod edit-dist ((seq1 number) (seq2 symbol) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (alpha>num seq1) (alpha>num seq2) change insert delete inex normalized test))

(defmethod edit-dist ((seq1 number) (seq2 number) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (alpha>num seq1) (alpha>num seq2) change insert delete inex normalized test))

(defun dimension-weights (wgth n)
  "The weights of n dimensions, summing to 1, from wgth: a number gives the same weight to all;
a list too short is completed with its last weight, a list too long is cut.
If all the weights are 0, all are 0 (every dimension is ignored)."
  (let ((w (cond ((numberp wgth) (make-list n :initial-element wgth))
                 ((< (length wgth) n)
                  (message (format nil "wgth has ~D weights for ~D dimensions: the last weight is used for the missing ones."
                                   (length wgth) n))
                  (append wgth (make-list (- n (length wgth)) :initial-element (or (car (last wgth)) 1))))
                 ((> (length wgth) n)
                  (message (format nil "wgth has ~D weights for ~D dimensions: the extra weights are ignored."
                                   (length wgth) n))
                  (subseq wgth 0 n))
                 (t wgth))))
    (if (zerop (apply #'+ w))
        w
        (normalize w))))

(defun edit-dist-ldl1 (seq1 seq2 &key (change 1) (insert 1) (delete 1) (wgth '(1 1)) (test #'equalp))
  (assert (and (apply #'= (mapcar #'length seq1))
	       (apply #'= (mapcar #'length seq2))
	       (= (length (car seq1)) (length (car seq2)))))
  (let* ((r 0)
	 (matrix1 (mat-transpose seq1))
	 (matrix2 (mat-transpose seq2))
	 (wgth (dimension-weights wgth (length (car seq1)))))
    (dotimes (n (length matrix1) r) ;(/ r (length (car seq1))))
      (incf r (* (nth n wgth)
		 (edit-dist1 (nth n matrix1) (nth n matrix2)
				   :change change :insert insert :delete delete :normalized 'T :test test))))))

(defun edit-dist-ldl2 (seq1 seq2 &key (change 1) (insert 1) (delete 1) (inex 0) (wgth '(1 1)) (test #'equalp))
  (assert (and (apply #'= (mapcar #'length seq1))
	       (apply #'= (mapcar #'length seq2))
	       (= (length (car seq1)) (length (car seq2)))))
  (let* ((r 0)
	 (matrix1 (mat-transpose seq1))
	 (matrix2 (mat-transpose seq2))
	 (wgth (dimension-weights wgth (length (car seq1)))))
    (dotimes (n (length matrix1) r) ;(/ r (length (car seq1))))
      (incf r (* (nth n wgth)
		 (edit-dist2 (nth n matrix1) (nth n matrix2)
			     :change change :insert insert :delete delete :inex inex :normalized 'T :test test))))))

(defgeneric multi-edit-dist (seq1 seq2 wgth &key change insert delete inex test)
  (:documentation
 "Considering each element of sequences seq1 and seq2 as an event described into symbolic dimensions,
returns the editing distance between seq1 and seq2 according to the weigthed dimensions defined.
By symbolic dimensions, I consider a structured list of 'arbitrary' symbols describing an event, such as, for instance,
 '(a IV x)."))

(defmethod multi-edit-dist ((seq1 list) (seq2 list) (wgth t)
			    &key (change 1) (insert 1) (delete 1) (inex 0) (test #'equalp))
  (if inex
      (if (zerop inex)
	  (edit-dist-ldl1 seq1 seq2 :change change :insert insert :delete delete :wgth wgth :test test)
	  (edit-dist-ldl2 seq1 seq2 :change change :insert insert :delete delete :inex inex :wgth wgth :test test))
      (edit-dist-ldl1 seq1 seq2 :change change :insert insert :delete delete :wgth wgth :test test)))

(defmethod multi-edit-dist ((seq1 list) (seq2 null) (wgth t)
			    &key (change 1) (insert 1) (delete 1) (inex 0) (test #'equalp))
  ;(assert (apply #'= (mapcar #'length seq1)))
  (let (r (l (length seq1)))
    (dotimes (i l (nreverse r))
      (dotimes (j (- l i 1))
        (push (list i (+ i j 1) (multi-edit-dist (nth i seq1) (nth (+ i j 1) seq1) wgth
                                                 :change change :insert insert :delete delete :inex inex :test test)) r)))))

(defgeneric commun-elt (set1 set2 &key test key)
  (:documentation "Returns common elements to both set1 and set2, nil if not."))

(defmethod commun-elt ((set1 list) (set2 list) &key test key)
  (when (not test) (setf test #'equalp))
  (when (not key) (setf key #'identity))
  (intersection set1 set2 :test test :key key))

(defmethod commun-elt ((set1 list) (set2 null) &key test key)
 (when (not test) (setf test #'equalp))
 (when (not key) (setf key #'identity))
 (let ((r nil) (l (length set1)))
  (dotimes (i l (nreverse r))
   (dotimes (j (- l i 1))
    (push (list i (+ i j 1) (intersection (nth i set1) (nth (+ i j 1) set1) :test test :key key)) r)))))

(defgeneric intersectioon-card (a b &key key test)
 (:documentation "Cardinal de l'intersection des ensembles a et b, ou de tous les éléments de a si b est vide."))

(defmethod intersection-card ((a list) (b list) &key (key #'identity) (test #'equalp))
  (length (intersection (remove-duplicates a :key key :test test)
                        (remove-duplicates b :key key :test test)
                        :key key :test test)))

(defmethod intersection-card ((a list) (b null) &key (key #'identity) (test #'equalp))
  (let ((x (pop a)))
    (loop until (not a)
          do (setf x (intersection (remove-duplicates x :key key :test test)
                                   (remove-duplicates (pop a) :key key :test test)
                                   :key key :test test)))
    (length x)))

(defgeneric jaccard-dist (a b &key test key)
  (:documentation "Jaccard distance.
If a is null, Jaccard distance of both lists of b ;
if b null, as usual in fv-morphologie, matrix of Jaccard distances of lists of a."))

(defmethod jaccard-dist ((a list) (b list) &key (test #'equalp) (key #'identity))
  ;; the union counted with key and test, like the intersection (it used equalp only);
  ;; two empty sets are identical: 0
  (let* ((j (intersection-card a b :test test :key key))
         (o (- (+ (length (remove-duplicates a :key key :test test))
                  (length (remove-duplicates b :key key :test test)))
               j)))
    (if (zerop o) 0.0 (float (/ (- o j) o)))))

(defmethod jaccard-dist ((a list) (b null) &key (test #'equalp) (key #'identity))
  (declare (ignore b))
  (let (r (l (length a)))
    (dotimes (i l (nreverse r))
      (dotimes (j (- l i 1))
        (push (list i (+ i j 1) (jaccard-dist (nth i a) (nth (+ i j 1) a)
                                              :key key :test test)) r)))))

(defmethod jaccard-dist ((a null) (b list) &key (test #'equalp) (key #'identity))
  (declare (ignore a))
  (let ((n (length (apply #'append b))))
    (if (zerop n) 0.0 (float (/ (intersection-card b nil :key key :test test) n)))))

;(jaccard-dist '((a b c d e f) (a b c 1 2 3) (b x y)) nil)
;(jaccard-dist 'nil '((a b c d e f) (a b c 1 2 3) (b x y)))
;(jaccard-dist '(1 0 1 0 0 0 0) '(1 0 0 1 0 1 1))

(defgeneric binary-jaccard-dist (a b &key key test)
  (:documentation "Jaccard distance for binary ensembles."))

(defmethod binary-jaccard-dist ((a list) (b list) &key (key #'identity) (test nil))
  (assert (= (length a) (length b)))
  (if test
      (let* ((e (mapcar #'(lambda (x y)
                           (list (funcall test (funcall key x)) (funcall test (funcall key y))))
                       a b))
             (tt (length (remove-if-not #'(lambda (x) (equalp x '(t t))) e)))
             (nn (length (remove-if-not #'(lambda (x) (equalp x '(nil nil))) e)))
             (tn (length (remove-if-not #'(lambda (x) (equalp x '(t nil))) e)))
             (nt (length (remove-if-not #'(lambda (x) (equalp x '(nil t))) e))))
        (declare (ignore nn))
        ;; no position present in either: identical, 0
        (if (zerop (+ nt tn tt)) 0.0 (float (/ (+ nt tn) (+ nt tn tt)))))
    (let* ((o (append a b))
           (bins (remove-duplicates (mapcar key o)))
           (b1 (length (remove (car bins) o :key key)))
           (b2 (length (remove (cadr bins) o :key key)))
           (min (if (< b2 b1) (car bins) (cadr bins))))
      (cond ((< (length bins) 2) 0.0)   ; one value everywhere (or nothing): identical
            ((> (length bins) 2)
             (error "binary-jaccard-dist requires binary values, not ~D different values." (length bins)))
            (t (binary-jaccard-dist a b :key key :test #'(lambda (x) (equalp x min))))))))

(defmethod binary-jaccard-dist ((a list) (b null) &key (key #'identity) (test nil))
  (let (r (l (length a)))
    (dotimes (i l (nreverse r))
      (dotimes (j (- l i 1))
        (push (list i (+ i j 1) (binary-jaccard-dist (nth i a) (nth (+ i j 1) a)
                                              :key key :test test)) r)))))

;(binary-jaccard-dist '(1 0 1 0 0 0 0) '(1 0 0 1 0 1 1))
;(binary-jaccard-dist '((a 1) (b 0) (c 1) (d 0) (e 0) (f 0) (g 0)) '((a 1) (b 0) (c 0) (d 1) (e 0) (f 1) (g 1)) :key #'cadr)
;(binary-jaccard-dist '(1 0 1 0 0 0 0) '(1 0 0 1 0 1 1) :test #'(lambda (x) (= 1 x)))
;(binary-jaccard-dist '(1 0 1 0 0 0 0) '(1 0 0 1 0 1 1) :test #'(lambda (x) (= 0 x)))

(defun sorensen-similarity (a b &key (test #'equalp) (key #'identity))
  "Sorensen-Dice similarity, 2 |a & b| / (|a| + |b|): 1 for identical sets (two empty sets too), 0 for disjoint sets."
  (let ((ca (length (remove-duplicates a :key key :test test)))
        (cb (length (remove-duplicates b :key key :test test))))
    (if (zerop (+ ca cb))
        1.0
        (float (/ (* 2 (intersection-card a b :test test :key key))
                  (+ ca cb))))))

(defun mountford-similarity (a b &key (key #'identity) (test #'equalp))
  "Mountford's index M = 2j / (2ab - (a+b)j) (a, b the sizes of the sets, j of their intersection),
normalized to M / (1 + M), from 0 (disjoint sets) to 1 (identical sets, two empty sets too):
M itself is infinite for identical sets."
  (let ((ca (length (remove-duplicates a :key key :test test)))
        (cb (length (remove-duplicates b :key key :test test)))
        (j (intersection-card a b :test test :key key)))
    (cond ((and (zerop ca) (zerop cb)) 1.0)
          ((zerop j) 0.0)
          (t (float (/ (* 2 j) (+ (- (* 2 ca cb) (* (+ ca cb) j)) (* 2 j))))))))

;(mountford-similarity '(1 0 1 0 0 0 0 2 3 4) '(1 0 0 1 0 1 1))

(defgeneric struct-dist (a b &optional w-occ w-rep test)
  (:documentation   "Dissemblance value (0 to 1) between sequences a and b according to their internal structures:
for each element, the number of its occurrences and the position of its first occurrence in its sequence,
compared with the multidimensional edit distance.
w-occ: weigth for 'occurence structure';
w-rep: weigth for 'repetition' structure;
test: identity of the elements (equalp by default)."))

(defmethod struct-dist ((a list) (b list) &optional (w-occ 1) (w-rep 1) (test #'equalp))
  ;; the occurrences and first positions are counted with test, for any kind of element
  ;; (match had methods for symbols and numbers only, and test compared the codes, not the elements)
  (flet ((codes (s)
           (mapcar #'(lambda (x) (list (count x s :test test) (position x s :test test))) s)))
    (multi-edit-dist (codes a) (codes b) (list w-occ w-rep) :inex 0 :test #'eql)))

(defmethod struct-dist ((a list) (b null) &optional (w-occ 1.) (w-rep 1.) (test #'equalp))
  (let ((r 'nil))
    (dotimes (i (length a) (reverse r))
      (dotimes (j (- (length a) (1+ i)))
	(push (list i (+ i j 1)
		    (struct-dist (nth i a) (nth (+ i j 1) a) w-occ w-rep test))
	      r)))))

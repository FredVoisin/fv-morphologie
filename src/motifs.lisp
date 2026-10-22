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

;;;; motifs.lisp
;;;;
;;;; Pattern structure: motifs (repeated subsequences) and groups of adjacent
;;;; elements.

(in-package :fv-morphologie)

;;; PATTERN STRUCTURE is old Structure 2 totally rewritten

(defgeneric find-pos (motif seq &optional diss l-var distance-fct)
  (:documentation "Yet another find pattern in a sequence with resemblance (default) criteria.
Returns the positions of sequence <ptrn> into sequence <seq> with some tolerance in comparing to be adjusted by optional arguments.
<motif> : motif to search;
<seq> : sequence of symbols.
Optional argument: distance function in lambda form, for instance : (lambda (a b) (edit-dist a b))."))

(defun edit-dist-bounded (a b start lb bound &optional (test #'equalp))
  "The edit distance (costs 1) between the vector a and the lb elements of the vector b from
position start, if it is not more than the integer bound, else nil. The computation stops as
soon as the distance is certainly more than bound: the minimum of a row of the matrix of costs
never decreases in the next rows."
  (declare (simple-vector a b) (fixnum start lb bound) (function test))
  (let ((la (length a)))
    (declare (fixnum la))
    (when (<= (abs (- la lb)) bound)
      (let ((prev (make-array (1+ la) :element-type 'fixnum))
            (row (make-array (1+ la) :element-type 'fixnum)))
        (declare (type (simple-array fixnum (*)) prev row))
        (dotimes (i (1+ la)) (setf (aref prev i) i))
        (loop for j fixnum from 1 to lb
              do (setf (aref row 0) j)
                 (let ((m j) (bj (svref b (+ start j -1))))
                   (declare (fixnum m))
                   (loop for i fixnum from 1 to la
                         do (let ((d (min (+ (aref prev (1- i))
                                             (if (funcall test (svref a (1- i)) bj) 0 1))
                                          (1+ (aref prev i))
                                          (1+ (aref row (1- i))))))
                              (declare (fixnum d))
                              (setf (aref row i) d)
                              (when (< d m) (setf m d))))
                   (when (> m bound) (return-from edit-dist-bounded nil)))
                 (rotatef prev row))
        (let ((d (aref prev la)))
          (when (<= d bound) d))))))

(defun find-pos-in (motif v start diss l-var distance-fct &optional (test #'equalp))
  "Positions (first last) of motif in the vector v, from position start (see find-pos).
Without distance-fct, the normalized edit distance is used, test being the identity of elements;
with diss 0 too, the windows are compared element by element with test, which gives the same
result without computing distances."
  (let* ((m (coerce motif 'simple-vector))
         (v (coerce v 'simple-vector))
         (l (length m))
         (lseq (length v))
         (l+ (floor (* l l-var)))   ; for length tolerance
         (exact (and (null distance-fct) (zerop diss)))
         ;; with a margin for the rounding of floats
         (rdiss (+ (rational diss) 1/1000000))
         pos)
    (flet ((match-p (i j)
             (if exact
                 (and (= j l)
                      (loop for k below l always (funcall test (aref m k) (aref v (+ i k)))))
                 (if distance-fct
                     (>= diss (funcall distance-fct motif (coerce (subseq v i (+ i j)) 'list)))
                     ;; the normalized edit distance, computed only while it may be <= diss;
                     ;; same decision as before
                     (let* ((maxlen (max l j))
                            (d (edit-dist-bounded m v i j (floor (* rdiss maxlen)) test)))
                       (and d (>= diss (float (/ d maxlen)))))))))
      (loop for i from start to (- lseq l+ 1)
            do (loop for j from (max 1 (- l l+)) to (+ l l+)
                     while (<= (+ i j) lseq)
                     when (match-p i j)
                       do (push (list i (+ i j -1)) pos)
                          (return))))
    (reverse (remove-if #'(lambda (x)
                            (some #'(lambda (a) (subpos-p x a))
                                  (remove x pos :test #'equalp)))
                        pos))))

(defmethod find-pos ((motif list) (seq list) &optional (diss 0) (l-var 0) (distance-fct nil))
  (find-pos-in motif (coerce seq 'vector) 0 diss l-var distance-fct))

; (time (find-pos '(a b c) '(t x a  b c y  t a e  b c k  r a b  c)))
; (time (find-pos  '(a b c) '(t x a b c y t a e b c k r a b c) .25 .34))
; (time (find-pos  '(a b c) '(t x a b c y t a e b c k r a b c) .25 .2 1 0 1 0))

(defun subseq-p (a b &optional (test #'equalp))
  (let ((la (length a))
        (lb (length b))
        (pos 0)
        x)
    (if (or (not a) (not b)) ; trivial test
	nil
      (if (< la lb)
          (loop for i from 0 to (- lb la)
                until (setf x (funcall test a (subseq b i (+ i la))))
                do (setf pos i)
                finally (incf pos))
        (if (> la lb)
            (loop for i from 0 to (- la lb)
                  until (setf x (funcall test b (subseq a i (+ i lb))))
                  do (setf pos i)
                  finally (incf pos))
          (if (equalp a b)  ; another trivial test
              (setf x t)
            (setf x (funcall test a b))))))
    (values (if x pos x))))

;(subseq-p '(a b c) '(x y a b c z))
;(subseq-p '(a b c) '(x y d a b e c z) #'(lambda (a b) (<= (edit-dist a b 1 1 1 0 T) .34)))
;(subseq-p '(a b c) '(x y d a b e c z) #'(lambda (a b) (<= (edit-dist a b 1 1 1 0 T) .25)))
;(subseq-p '(a b c) '(x y a b z))
;(subseq-p '(a b c) '(x y a c d z) #'(lambda (a b) (<= (- (char-code (elt (format nil "~S" a) 0)) (char-code (elt (format nil "~S" b) 0))) 1)))

(defun whenlooploopwhenwhen (seq &optional (diss 0) (l-var 0) (n nil) (distance-fct nil) (test #'equalp))
  (let ((r (list))
	(n- 2)
	(n+ nil)
	(l (length seq))
	(v (coerce seq 'vector)))
    ;; distance-fct nil: the normalized edit distance (see find-pos-in)
    (when (not n) (setf n (list 3 (floor (/ l 2)))))
    (if (numberp n)
        (setf n+ n)
      (setf n- (car n)
            n+ (cadr n)))
    (loop for i from 0 to (- l (max 2 n-)) 
       do (loop for j from n- to (if n+ (min n+ (- l i)) (- l i))
	     do (let (f (candidat (subseq seq i (+ i j))))
		  ;; skip a candidate identical to an earlier motif, or which is one of its repetitions
		  (when (not (or (assoc candidat r :test #'equalp)
				 (let ((range (list i (+ i j -1))))
				   (some #'(lambda (x) (member range (cdr x) :test #'equal)) r))))
		    (setf f (find-pos-in candidat v (+ i j) diss l-var distance-fct test)))
		  (when f (push (append (list candidat) (cons (list i (+ i j -1)) f)) r)))))
    (remove-if #'(lambda (x)
		   (some #'(lambda (a) (l-subpos-p (cdr x) (cdr a)))
			 (remove x r :test #'equalp)))
	       r)))

#|
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
			     0 0 '(3 5)))
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
			     0.25 0.25 '(3 6)))
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
            ;;                 0 1 2  3 4 5  6      9     12     15     18     21     24     27     30
			     0.25 0.25 '(3 8))
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
			      .0 .0 nil)
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
			     0 .4 nil)
|#

;; next function does also an brut-force run on the seqs which may be not well adapted to huge lists
;; for huge lists of symbols, I rather like to do the same but using an random search to be tuned - kind of sondage or windowing -
;; in the list, following more flexible or fuzzy/lazy rules for probabiltistic retrieval - may be use of a dict ?

(defgeneric find-self (seq out &optional diss l-var n distance-fct test)
  (:documentation "List all patterns in seq found using ptrn-find."))

(defmethod find-self ((seq list) (out symbol) &optional (diss 0) (l-var 0) (n nil) (distance-fct nil) (test #'equalp))
  ;; distance-fct nil: the normalized edit distance with test (see find-pos-in)
  (when (null diss) (setf diss 0))
  (when (null l-var) (setf l-var 0))
  (when (null out) (setf out ':length))
  (case out
    (:length
     (sort (whenlooploopwhenwhen seq diss l-var n distance-fct test) '> :key (lambda (x) (length (car x)))))
    (:freq
     (sort (whenlooploopwhenwhen seq diss l-var n distance-fct test) '> :key (lambda (x) (length (cdr x)))))
    (t (whenlooploopwhenwhen seq diss l-var n distance-fct test))))

;(print (find-self '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :length))
;(print (find-self '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :freq))

(defun insert-no-overlap (seq i &optional (test #'(lambda (x) (> x 0))))
  "i liste (min max). Les bornes - car et cadr - ne se recouvrent pas. Test par defaut : (lambda (x) (> x 0)) "
  (let ((diffmin (mapcar #'(lambda (x) (- (car i) (cadr x))) seq))
        (diffmax (mapcar #'(lambda (x) (- (car x) (cadr i))) seq))
        a b)
    (when (not test) (setf test #'(lambda (x) (> x 0))))
    (if (and (setf a (position-if test diffmin :from-end t))
             (setf b (position-if test diffmax :from-end nil)))
        (append (subseq seq 0 (1+ a)) (list i) (subseq seq b))
      (if a
          (if (= a (1- (length seq)))
              (append seq (list i))
            seq)
        (append (list i) seq)))))

;(insert-range '((0 10) (20 25) (30 32)) '(27 29))
;(insert-range '((0 10) (20 25) (30 32)) '(11 19))
;(insert-range '((0 10) (20 25) (30 32)) '(25 30))
;(insert-range '((0 10) (20 25) (30 32)) '(25 30) #'(lambda (x) (>= x 0)))
;(insert-range '((0 10) (20 25) (30 32)) '(33 39))
;(insert-range '((0 10) (20 25) (30 32)) '(-8 -1))
;(insert-range '((0 10) (20 25) (30 32)) '(0 5))
;(insert-range '((0 10) (20 25) (30 32) (35 39)) '(30 33)) 
;(insert-range '() '(0 9))

(defun insert-all (seq i &optional test)
  "i liste (min max).  Les bornes - car et cadr - peuvent se recouvrir."
  (declare (ignore test))
  (let ((s (sort (append seq (list i)) '< :key #'car)))
    (sort s '< :key #'cadr)))

;(insert-all '((0 10) (20 25) (30 32) (35 39)) '(31 36))
;'((0 10) (20 25) (30 (31 32) (35 36) 39))

;(cdr '(0 2 (0 4)))
;(cdr '(10 (13 15) 19))

(defun insert-tree (seq i &optional test)
  ""
  (let ((diffmin (mapcar #'(lambda (x) (- (car i) (cadr x))) seq))
        (diffmax (mapcar #'(lambda (x) (- (car x) (cadr i))) seq))
        a b)
    (when (not test) (setf test #'(lambda (x) (> x 0))))
    (if (and (setf a (position-if test diffmin :from-end t))
             (setf b (position-if test diffmax :from-end nil)))
        (append (subseq seq 0 (1+ a)) (list i) (subseq seq b))
      (if a
          (if (= a (1- (length seq)))
              (append seq (list i))
            seq)
        (append (list i) seq)))))

; (((T X A B C) (0 4) (16 20))
; ((B C K R) (9 12) (29 32))
; ((Y T A E) (5 8) (22 25))
; ((T X A) (0 2) (13 15) (16 18)))

(defgeneric rep-strct (seq &key diss l-var n criteria distance-fct test)
  (:documentation "Possibles structures of seq based of automatically found patterns (plist)."))

(defmethod rep-strct ((seq t) &key (diss 0) (l-var 0) (n nil) (criteria :length) distance-fct (test #'equalp))
  ;; without distance-fct, the normalized edit distance with test (with diss 0: element by element)
  (let* ((x (whenlooploopwhenwhen seq diss l-var n distance-fct test))
         (ptrns
          (case criteria
            (:length
             (remove-duplicates (sort x '> :key #'(lambda (x) (length (car x))))
                                :from-end t
                                :test #'(lambda (a b) (and (subseq-p (car a) (car b)) (= (length (cdr a)) (length (cdr b)))))))
            (:freq
             (remove-duplicates (sort x '> :key #'(lambda (x) (length (cdr x))))
                                :from-end nil
                                :test #'(lambda (a b) (and (subseq-p (car a) (car b)) (= (length (cdr a)) (length (cdr b)))))))
            (t (remove-duplicates x
                                  :from-end t
                                  :test #'(lambda (a b) (and (subseq-p (car a) (car b)) (= (length (cdr a)) (length (cdr b)))))))))
         (pos (sort (remove-duplicates (apply #'append (mapcar #'(lambda (x) (mapcar #'car (cdr x))) ptrns))) '<))
         (ptrn-list nil)
         (st nil))
    (mapcar #'(lambda (x) (setf (cdr x) (mapcar #'car (cdr x)))) ptrns)
    (dotimes (k (length ptrns))
      (push (list (car (nth k ptrns)) k) ptrn-list))
    (dolist (p pos)
      (push (apply #'append (list (list p)
                                  (mapcar #'(lambda (x) (cadr (assoc (car x) ptrn-list :test #'equalp)))
                                          (remove-if-not #'(lambda (x) (member p (cdr x))) ptrns))))
            st))
    (list (reverse st) (mapcar #'car (sort ptrn-list '< :key #'cadr)))))
    
#|
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria :length))
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria :freq)
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria nil)
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria :freq
                  :diss 0.25 :l-var 0.3 :n '(2 8)))
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria nil
                  :diss 0.25 :l-var 0.3 :n '(2 8)))
|#

;; groupadjif1 / 2 a optimiser !

(defun groupadjif1 (data test r)
  "Groups adjacent elts in data if test is verified. Elts are atoms."
  (if (null data)
      (reverse (mapcar #'(lambda (x) (if (listp x) (-reverse x nil) x)) r))
    (let ((x (pop data)))
      (groupadjif1 data test
		   (if r
		       (let* ((c (car r)) (y (if (listp c) (car c) c)))
			 (if (funcall test y x)
			     (if (listp c)
				 (cons (cons x c) (cdr r))
				 (cons (cons x c) (cdr r)))
			     (cons x r)))
		       (cons x r))))))

;(groupadjif1 '(1 2 3 3 4 5 6 6 6 6 7 8 9) #'= nil) ;duscutable cf v if2

(defun groupadjif2 (data test r)
  (if (null data)
      (nreverse r) ;(mapcar #'(lambda (x) (apply #'append x)) r))
     (let ((x (pop data)))
      (groupadjif2 data test
                   (if r
                       (let* ((c (car r)) ); (y (if (listp (car c)) (car c) c)))
                         ;(print (list c x r))
                         (if (funcall test c x) ;y
                             (cons (append c x) (cdr r))
                           (cons x r)))
                     (cons x r))))))

;(groupadjif2 '((0 1) (1 8) (2 1) (3 8) (4 8) (5 8) (9 1) (9 1) (9 2) (1 1)) #'(lambda (x y) (= (cadr x) (cadr y))) nil)

(defgeneric adj-group (seq test &optional key)
  (:documentation "Groups adjacent elts in data according to the function test ('eq by default).
A function key specifies the position or value to be tested for each element of seq with optional argument.
If no key (by default), the test apply to the element itself."))

(defmethod adj-group ((seq list) (test function) &optional (key nil))
  (when (not (functionp test)) (setf test #'equalp)
    (message "adj-group: test is not a function, using test #'equalp"))
  (cond ((not (member nil (mapcar #'atom seq)))
         (if (not key)
             (groupadjif1 seq test nil)
           (groupadjif1 seq #'(lambda (x y) (funcall test (funcall key x) (funcall key y))) nil)))
        ((not (member nil (mapcar #'listp seq)))
         (when (not key) (setf key #'(lambda (x) (remove-duplicates x :test #'equalp)))) ; #'identity))
         (groupadjif2 seq #'(lambda (x y) (funcall test (funcall key x) (funcall key y))) nil))) )

(defmethod adj-group ((seq list) (test t) &optional (key nil))
  (adj-group seq #'equalp key))

;(adj-group '(0 1 2 3 3 3 4 3 2 1 1 1 1 2 1) #'=)
;(adj-group '((0 1) (1 8) (2 1) (3 8) (4 8) (5 8) (9 1) (9 1) (9 2) (1 1)) #'(lambda (x y) (= (cadr x) (cadr y))) )
;(adj-group '((0 1) (1 8) (2 1) (3 8) (4 8) (5 8) (9 1) (9 1) (9 2) (1 1)) #'(lambda (x y) (= x y)) #'cadr)

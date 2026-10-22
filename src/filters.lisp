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

;;;; filters.lisp
;;;;
;;;; Filters on sequences: noise, local repetitions, sliding windows, median,
;;;; smoothing, extrema.

(in-package :fv-morphologie)

(defgeneric fnoise (list &optional mode val)
 (:documentation "noise based on variation"))

(defun random-positive (q)
  "A random float in ]0, q[."
  (loop for r = (random (float q)) until (plusp r) finally (return r)))

(defmethod fnoise ((list list) &optional mode val)
  "Noise on list according to mode, with amount val (0.1 by default):
:nozero replaces each zero by a random value in ]0, val * range[ (]0, val[ if all values are equal);
:swap swaps val * length pairs of elements (val integer: number of swaps);
:relative adds to each value a random amount in [-val * range, val * range].
The range is max - min of list. A list which is not numerical is swapped (:swap).
If mode is a function of two arguments (x l), each element x of list is replaced by its result,
l being (a copy of) the whole list, shared by all the calls (it should not be modified)."
  (let ((val (or val 0.1)))
    (cond ((functionp mode)
           (let ((l (copy-list list)))
             (mapcar #'(lambda (x) (funcall mode x l)) list)))
          ((null list) nil)
          ((notevery #'numberp list) (randsort list val))
          (t (let ((range (- (reduce #'max list) (reduce #'min list))))
               (case mode
                 (:nozero
                  (let ((q (if (plusp range) (* val range) val)))
                    (unless (plusp q)
                      (error "filt-noise :nozero: val must be positive, not ~S." val))
                    (mapcar #'(lambda (x) (if (zerop x) (random-positive q) x)) list)))
                 (:swap (randsort list val))
                 (:relative
                  (let ((q (float (* val range))))
                    (mapcar #'(lambda (x) (+ x (* q (- (random 2.0) 1.0)))) list)))
                 (t (error "filt-noise: unknown mode ~S, use :nozero, :swap, :relative or a function." mode))))))))

#|
; deep-tree to do
(defmethod fnoise ((x list) &optional (quasi-zero .001))
  (if (null x) nil
    (if (numberp x)
        (a-zero x quasi-zero)
      (cons (a-zero (first x) quasi-zero)
            (a-zero (rest x) quasi-zero)))))
|#

(defun remove-local-dup-rec (liste test r) ; ex ptrn-smooth
  "Returns the list LIST without local repetitions (r: elements already kept, most recent first).
nil is an element like any other."
  (let ((started (and r t))
        (last (car r)))
    (dolist (x liste (nreverse r))
      (unless (and started (funcall test x last))
        (push x r)
        (setf last x started t)))))

(defgeneric rem-local-rep (seq &optional test)
  (:documentation "Removes local repetitions.
 A condition may be specified with optional argument test (default: equalp)."))

(defmethod rem-local-rep ((seq list) &optional (test #'equalp))
  (remove-local-dup-rec seq test nil))

(defmethod rem-local-rep ((seq t) &optional test)
  (declare (ignore test)) 
  seq)

; fonction generique pour appliquer une fonction arbitraire sur une liste selon une fenetre glissante de taille (plus ou moins) determinee (pouvant varier dynamiquement ?) remplace 

(defun window-block (result v start end bstart bend)
  "The values given for the block [bstart, bend) of v by the result of a function applied
to the window [start, end): an atom (or nil) is repeated for the block; a list replaces
the block if the window is the block, otherwise it must be as long as the window and
gives the values at the positions of the block."
  (cond ((atom result) (make-list (- bend bstart) :initial-element result))
        ((and (= start bstart) (= end bend)) (copy-list result))
        ((= (length result) (- end start))
         (loop for p from bstart below bend
               collect (if (and (>= p start) (< p end))
                           (nth (- p start) result)
                           (aref v p))))
        (t (error "The function returned a list of ~D values for a window of ~D values."
                  (length result) (- end start)))))

(defun window-apply (seq func wind &optional (step 1))
  "Applies func to a window of wind values of seq.
With step 1, the window is centred on each value (an even wind is enlarged to the next
odd size) and shrinks symmetrically at the edges, so the first and last values are kept.
With a larger step, func is applied to a window of wind values centred on each block of
step values. See window-block for the values given by the result of func."
  (let* ((v (coerce seq 'vector))
         (l (length v))
         (wind (max 1 wind))
         (step (max 1 step)))
    (flet ((apply-to (start end bstart bend)
             (window-block (funcall func (coerce (subseq v start end) 'list))
                           v start end bstart bend)))
      (if (= step 1)
          (let ((h (floor (if (evenp wind) (1+ wind) wind) 2)))
            (loop for i below l
                  append (let ((k (min h i (- l 1 i))))
                           (apply-to (- i k) (+ i k 1) i (1+ i)))))
          (loop for n from 0 below l by step
                append (let* ((start (max 0 (- n (floor (- wind step) 2))))
                              (end (min l (+ start wind))))
                         (apply-to start end n (min l (+ n step)))))))))

;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'mean 3 3)
;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'mean 4 1)
;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'mean 4)
;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'(lambda (x) (nth (random (length x)) x)) 4)
;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'(lambda (x) (loop repeat (length x) collect (nth (random (length x)) x))) 2 2)

;(defun resample (data n &optional mode)
;  (let ((l (length data)))
;    ))

(defun resample-binary (data n)
  (let ((l (length data))
        (r (list)))
    (if (= n l)
        data  (let ((k (/ l n)))
		(dotimes (a n (nreverse r))
		  (push (nth (min (round (* a k)) (1- l)) data) r))))))

(defun median-simple (list w)
  "Sliding median over w values centred on each value of list, w odd: the median is always a value of list.
At the edges, the first and last values are repeated. The result has the same length as list."
  (let* ((v (coerce list 'vector))
         (n (length v))
         (h (floor w 2)))
    (loop for i below n
          collect (nth h (sort (loop for k from (- i h) to (+ i h)
                                     collect (aref v (max 0 (min (1- n) k))))
                               #'<)))))

(defun median (list w) ;; ???
  (let ((r nil) ) ; (i  (1+ (floor (/ w 2)))))
    (loop until (null list)
       do (let (z (window (loop repeat w collect (if list (pop list) (car r)))))
            (setf z (sort window '<))
            (setf r (append r (remove-if #'(lambda (x) (or (= (car z) x) (= (car (last z)) x))) window)))))
    (reverse r)))

(defgeneric median-filter (seq window)
  (:documentation "Median filter."))

(defmethod median-filter ((seq list) (window integer))
  "Median filter over an odd window: an even window is enlarged to the next odd size."
  (if (< window 2)
      (copy-list seq)
      (median-simple seq (if (evenp window) (1+ window) window))))

(defmethod median-filter ((seq list) (window real))
  (median-filter seq (round window)))

(defun exponential-smoothing (data alpha)
  "s(t) = s(t-1) + alpha (x(t) - s(t-1)), s(0) = x(0)."
  (when data
    (let ((r (list (car data))))
      (dolist (x (cdr data) (reverse r))
        (push (+ (car r) (* alpha (- x (car r)))) r)))))

(defun double-exponential-smoothing (data alpha gamma)
  "Holt's double exponential smoothing, with level s and trend b:
s(t) = alpha x(t) + (1 - alpha) (s(t-1) + b(t-1))
b(t) = gamma (s(t) - s(t-1)) + (1 - gamma) b(t-1)
from s(0) = x(0) and b(0) = x(1) - x(0)."
  (if (null (cdr data))
      (copy-list data)
      (let* ((s (car data))
             (b (- (cadr data) (car data)))
             (r (list s)))
        (dolist (x (cdr data) (nreverse r))
          (let ((s1 (+ (* alpha x) (* (- 1 alpha) (+ s b)))))
            (setf b (+ (* gamma (- s1 s)) (* (- 1 gamma) b))
                  s s1)
            (push s r))))))

;;filtre extreme (contraire de median)

(defun extrema (list w &optional (minmax :max))
  (let ((r nil)) ; (i  (1+ (floor (/ w 2)))))
    (loop until (null list)
       do (let ((window (loop for k from 0 to w
			   collect (let ((z (nth k list))) (if z z (car r))))))
            (case minmax
              (:max (push (car (sort window '>)) r))
              (:min (push (car (sort window '<)) r))
              (t (push (car (sort window '>)) r))))
	 (pop list))
    (nreverse r)))

(defgeneric extrema-filter (seq window)
  (:documentation "Extrema filter."))

(defmethod extrema-filter ((seq list) (window integer))
  (if (<= (length seq) window)
      seq (values (extrema seq window))))

(defmethod extrema-filter ((seq list) (window null))
  (values (extrema seq (max 3 (floor (/ (length seq) 100))))))

(defun nocons= (seq &optional (test #'=))
  "Reechantillonne seq en y interpolant lineairement les valeurs consecutives egales :
each run of values equal (by test) to its first value x is replaced by a ramp from x to the
next different value; the last run is kept. seq is not modified."
  (let* ((v (coerce seq 'vector))
         (l (length v))
         (r (copy-seq v))
         (k 0))
    (loop while (< k l)
          do (let ((z (loop for n from 1
                            while (and (< (+ k n) l) (funcall test (aref v (+ k n)) (aref v k)))
                            finally (return n))))
               (when (and (> z 1) (< (+ k z) l))
                 (let ((d (float (/ (- (aref v (+ k z)) (aref v k)) z))))
                   (dotimes (n z)
                     (setf (aref r (+ k n)) (+ (aref v k) (* n d))))))
               (incf k z)))
    (coerce r 'list)))

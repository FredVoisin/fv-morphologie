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

;;;; signature.lisp
;;;;
;;;; Signatures of numeric sequences: major extrema, local extrema, landmarks,
;;;; plateaus and inflexions, and their output (exsample).

(in-package :fv-morphologie)

;; TOOLS for NUMERIC SERIES
;; the signature of a sequence of numeric values is made of its salient points (position type value):
;; minima (-1), maxima (1) and inflexions (0), where a plateau begins or ends.

(defun major-extrema (seq r &optional (dist #'(lambda (a b) (abs (- a b)))))
  "Important extrema of seq (Fink & Gandhi, 2007): a_m is an important minimum if there are i < m < j
such that a_m is the minimum of a_i..a_j, dist(a_m, a_i) >= r and dist(a_m, a_j) >= r (and so for maxima).
With r = 0, every strict local extremum. Linear algorithm of Fink & Gandhi, which relaxes the definition
at the ends: an extremum between an end and a variation of at least r needs no variation of r on the side
of that end. Of equal extrema, the first is given (an extremum equal to an end is that end). Returns the
inner extrema as (position type value), type -1 for a minimum and 1 for a maximum (the endpoints are not
returned). r is a non-negative number, dist a distance: dist(a, a) is 0."
  (unless (and (realp r) (>= r 0))
    (error "major-extrema: the threshold must be a non-negative number, not ~S." r))
  (let* ((v (coerce seq 'vector))
         (last (1- (length v)))
         (out nil))
    ;; each search moves forward at least one position, so that it ends whatever dist
    (labels ((near (a b) (let ((d (funcall dist a b))) (if (zerop r) (<= d 0) (< d r))))
             (emit (i type) (when (< 0 i last) (push (list i type (aref v i)) out)))
             (find-first ()
               (let ((i 0) (imin 0) (imax 0))
                 (loop do (incf i)
                          (when (< (aref v i) (aref v imin)) (setf imin i))
                          (when (> (aref v i) (aref v imax)) (setf imax i))
                       while (and (< i last)
                                  (near (aref v i) (aref v imax))
                                  (near (aref v i) (aref v imin))))
                 (if (< imin imax) (emit imin -1) (emit imax 1))
                 i))
             (find-minimum (i)
               (let ((imin i))
                 (loop do (incf i)
                          (when (< (aref v i) (aref v imin)) (setf imin i))
                       while (and (< i last) (near (aref v i) (aref v imin))))
                 (when (or (< i last) (< (aref v imin) (aref v i))) (emit imin -1))
                 i))
             (find-maximum (i)
               (let ((imax i))
                 (loop do (incf i)
                          (when (> (aref v i) (aref v imax)) (setf imax i))
                       while (and (< i last) (near (aref v imax) (aref v i))))
                 (when (or (< i last) (> (aref v imax) (aref v i))) (emit imax 1))
                 i)))
      (when (> last 0)
        (let ((i (find-first)))
          (when (and (< i last) (< (aref v i) (aref v 0)))
            (setf i (find-minimum i)))
          (loop while (< i last)
                do (setf i (find-maximum i))
                   (when (< i last) (setf i (find-minimum i))))))
      (nreverse out))))

(defun local-extrema (seq)
  "The inner local extrema of seq, where the variation changes direction, as (position type value):
type -1 for a minimum, 1 for a maximum. A flat extremum is given at its first position."
  (let* ((v (coerce seq 'vector))
         (l (length v))
         (out nil)
         (dir 0)       ; direction of the last variation: -1, 0 (none yet) or 1
         (start 0))    ; first position of the current run of equal values
    (loop for i from 1 below l
          do (let ((d (signum (- (aref v i) (aref v (1- i))))))
               (unless (zerop d)
                 (when (= d (- dir))
                   (push (list start (if (> dir 0) 1 -1) (aref v start)) out))
                 (setf dir d
                       start i))))
    (nreverse out)))

(defun difference-function (diff)
  "The difference between two values a and b given by diff: nil or :absolute (|b - a|), :relative
(2 |b - a| / (|a| + |b|), the percentage of Perng et al.) or a function (a b)."
  (cond ((member diff '(nil :absolute)) #'(lambda (a b) (abs (- b a))))
        ((eq diff :relative)
         #'(lambda (a b)
             (let ((m (+ (abs a) (abs b))))
               (if (zerop m) 0 (/ (* 2 (abs (- b a))) m)))))
        ((functionp diff) diff)
        (t (error "exsample: :diff must be :absolute, :relative or a function (a b), not ~S." diff))))

(defun landmarks (seq thres &optional d (diff #'(lambda (a b) (abs (- b a)))))
  "Landmarks of seq (Perng, Parker & Leung, 1999): the first-order landmarks (local extrema), smoothed
by MDPP: a pair of consecutive landmarks (x1 y1) (x2 y2) is removed while x2 - x1 < d and their
difference (diff y1 y2) is below thres, the pair of smallest difference first (the left-most of equal
differences; after a removal, the landmarks before and after the pair become consecutive). d is nil
(no limit) or a non-negative number of positions.
Returns (position type value), type -1 for a minimum and 1 for a maximum."
  (unless (or (null d) (and (realp d) (>= d 0)))
    (error "landmarks: the distance D must be nil or a non-negative number, not ~S." d))
  (let* ((lm (coerce (local-extrema seq) 'vector))
         (n (length lm))
         (prev (make-array n))
         (next (make-array n))
         (alive (make-array n :initial-element t))
         (heap (make-array 16 :adjustable t :fill-pointer 0)))
    (dotimes (i n)
      (setf (aref prev i) (if (> i 0) (1- i) nil)
            (aref next i) (if (< i (1- n)) (1+ i) nil)))
    ;; heap of the removable pairs (difference i j), j following i when the pair was made;
    ;; the smallest difference first, then the left-most pair
    (labels ((before-p (e f)
               (or (< (first e) (first f))
                   (and (= (first e) (first f)) (< (second e) (second f)))))
             (heap-push (e)
               (vector-push-extend e heap)
               (loop with k = (1- (fill-pointer heap))
                     while (> k 0)
                     do (let ((up (floor (1- k) 2)))
                          (if (before-p (aref heap k) (aref heap up))
                              (progn (rotatef (aref heap k) (aref heap up)) (setf k up))
                              (return)))))
             (heap-pop ()
               (let ((top (aref heap 0))
                     (last (vector-pop heap)))
                 (when (plusp (fill-pointer heap))
                   (setf (aref heap 0) last)
                   (loop with k = 0
                         with m = (fill-pointer heap)
                         do (let* ((l (1+ (* 2 k))) (r (1+ l)) (best k))
                              (when (and (< l m) (before-p (aref heap l) (aref heap best))) (setf best l))
                              (when (and (< r m) (before-p (aref heap r) (aref heap best))) (setf best r))
                              (if (= best k)
                                  (return)
                                  (progn (rotatef (aref heap k) (aref heap best)) (setf k best))))))
                 top))
             (consider (i j)
               (when (and i j)
                 (let ((a (aref lm i)) (b (aref lm j)))
                   (when (or (null d) (< (- (first b) (first a)) d))
                     (let ((dv (funcall diff (third a) (third b))))
                       (when (< dv thres) (heap-push (list dv i j)))))))))
      (dotimes (i (1- n)) (consider i (1+ i)))
      (loop while (plusp (fill-pointer heap))
            do (destructuring-bind (dv i j) (heap-pop)
                 (declare (ignore dv))
                 ;; a pair is still there when both landmarks are, and still consecutive
                 (when (and (aref alive i) (aref alive j) (eql (aref next i) j))
                   (let ((p (aref prev i)) (q (aref next j)))
                     (setf (aref alive i) nil (aref alive j) nil)
                     (when p (setf (aref next p) q))
                     (when q (setf (aref prev q) p))
                     (consider p q)))))
      (loop for i below n when (aref alive i) collect (aref lm i)))))

(defun plateaus (seq thres duration)
  "The plateaus of seq: the stretches of at least duration positions where seq varies by less than thres
(where it is constant when thres is 0), as (start end), found from left to right, each as long as possible."
  (let* ((v (coerce seq 'vector))
         (n (length v))
         (out nil)
         (i 0))
    (flet ((flat-p (lo hi) (if (zerop thres) (= lo hi) (< (- hi lo) thres))))
      (loop while (< i (1- n))
            do (let ((j i) (lo (aref v i)) (hi (aref v i)))
                 (loop while (< (1+ j) n)
                       do (let ((x (aref v (1+ j))))
                            (if (flat-p (min lo x) (max hi x))
                                (setf lo (min lo x) hi (max hi x) j (1+ j))
                                (return))))
                 (if (>= (1+ (- j i)) duration)
                     (progn (push (list i j) out)
                            (setf i (1+ j)))
                     (incf i)))))
    (nreverse out)))

(defun minflexmax (seq thres &optional duration)
  "Minima and maxima of seq (the important extrema for thres, see major-extrema), and inflexions where a
plateau begins and where it ends: a plateau is a stretch of at least duration positions (2 by default)
where seq varies by less than thres (where it is constant when thres is 0). A plateau which contains an
extremum is a flat extremum and gives no inflexion. Returns (position type value), type -1 for a minimum,
1 for a maximum and 0 for an inflexion (the endpoints are not returned)."
  (unless (or (null duration) (and (integerp duration) (>= duration 0)))
    (error "minflexmax: the duration of a plateau must be nil or a number of positions, not ~S." duration))
  (let* ((v (coerce seq 'vector))
         (last (1- (length v)))
         (extrema (major-extrema seq thres))
         (points extrema))
    (dolist (p (plateaus seq thres (max 2 (or duration 2))))
      (destructuring-bind (start end) p
        (unless (find-if #'(lambda (x) (<= start (first x) end)) extrema)
          (dolist (i (list start end))
            (when (and (< 0 i last) (not (assoc i points)))
              (push (list i 0 (aref v i)) points))))))
    (sort (copy-list points) #'< :key #'first)))

;; the signature of seq and its output (exsample)

(defun droite (a b)
  (if (= (cadr a) (cadr b))
      (list 0 (cadr a))
    (let ((pente (/ (- (cadr a) (cadr b)) (- (car a) (car b)))))
      (list pente (- (cadr a) (* pente (car a)))))))

(defun signature (seq method thres &key diff duration out)
  "The signature of seq by method (:minmax, :minflexmax, :major-extrema or :landmarks), with the
threshold thres, the difference diff (:major-extrema and :landmarks) and the duration (:minflexmax and
:landmarks), given according to out: :signature (the points (position type value)), or with the first
and last values of seq, :values, :positions, :all ((position value)) or :resampled."
  (unless (member method '(:minmax :minflexmax :major-extrema :landmarks))
    (error "exsample: unknown method ~S, use :minmax, :minflexmax, :major-extrema or :landmarks." method))
  (unless (member out '(:signature :values :positions :all :resampled))
    (error "exsample: unknown output ~S, use :values, :positions, :all, :resampled or :signature." out))
  (unless (and (realp thres) (>= thres 0))
    (error "exsample: the threshold must be a non-negative number, not ~S." thres))
  (when (and diff (member method '(:minmax :minflexmax)))
    (error "exsample: :diff is not used by ~S." method))
  (when (and duration (member method '(:minmax :major-extrema)))
    (error "exsample: :duration is not used by ~S." method))
  (let ((points (when seq
                  (ecase method
                    (:minmax (major-extrema seq thres))
                    (:minflexmax (minflexmax seq thres duration))
                    (:major-extrema (major-extrema seq thres (difference-function diff)))
                    (:landmarks (landmarks seq thres duration (difference-function diff)))))))
    (if (or (eq out :signature) (null seq))
        points
        (let* ((v (coerce seq 'vector))
               (pos (remove-duplicates (append (list 0) (mapcar #'car points) (list (1- (length v)))))))
          (ecase out
            (:values (mapcar #'(lambda (x) (aref v x)) pos))
            (:positions pos)
            (:all (mapcar #'(lambda (x) (list x (aref v x))) pos))
            (:resampled
             ;; the straight lines between the points, at every position
             (let ((r nil))
               (loop for (xa xb) on pos
                     while xb
                     do (let* ((ya (aref v xa))
                               (d (droite (list xa ya) (list xb (aref v xb)))))
                          (push ya r)
                          (loop for j from (1+ xa) below xb
                                do (push (+ (* (car d) j) (cadr d)) r))))
               (push (aref v (1- (length v))) r)
               (nreverse r))))))))

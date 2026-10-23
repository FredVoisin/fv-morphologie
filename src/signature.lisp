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
;;;; Signatures of numeric sequences: primitives (contour features), major
;;;; extrema, landmarks, sampling.

(in-package :fv-morphologie)

;; TOOLS for NUMERIC SERIES
;; the signature of a sequence of numeric values is made of its salient points (position type value):
;; minima (-1), maxima (1) and inflexions (0), where a plateau begins or ends.

(defun major-extrema (seq r &optional (dist #'(lambda (a b) (abs (- a b)))))
  "Important extrema of seq (Fink & Gandhi, 2007): a_m is an important minimum if there are i < m < j
such that a_m is the minimum of a_i..a_j, dist(a_m, a_i) >= r and dist(a_m, a_j) >= r (and so for maxima).
With r = 0, every strict local extremum. Linear algorithm of Fink & Gandhi; returns the inner extrema
as (position type value), type -1 for a minimum and 1 for a maximum (the endpoints are not returned)."
  (let* ((v (coerce seq 'vector))
         (last (1- (length v)))
         (out nil))
    (labels ((near (a b) (let ((d (funcall dist a b))) (if (zerop r) (<= d 0) (< d r))))
             (emit (i type) (when (< 0 i last) (push (list i type (aref v i)) out)))
             (find-first ()
               (let ((i 0) (imin 0) (imax 0))
                 (loop while (and (< i last)
                                  (near (aref v i) (aref v imax))
                                  (near (aref v i) (aref v imin)))
                       do (incf i)
                          (when (< (aref v i) (aref v imin)) (setf imin i))
                          (when (> (aref v i) (aref v imax)) (setf imax i)))
                 (if (< imin imax) (emit imin -1) (emit imax 1))
                 i))
             (find-minimum (i)
               (let ((imin i))
                 (loop while (and (< i last) (near (aref v i) (aref v imin)))
                       do (incf i)
                          (when (< (aref v i) (aref v imin)) (setf imin i)))
                 (when (or (< i last) (< (aref v imin) (aref v i))) (emit imin -1))
                 i))
             (find-maximum (i)
               (let ((imax i))
                 (loop while (and (< i last) (near (aref v imax) (aref v i)))
                       do (incf i)
                          (when (> (aref v i) (aref v imax)) (setf imax i)))
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
                 (when (and (/= dir 0) (/= d dir) (> start 0))
                   (push (list start (if (> dir 0) 1 -1) (aref v start)) out))
                 (setf dir d))
               (when (/= (aref v i) (aref v (1- i))) (setf start i))))
    (nreverse out)))

(defun landmarks (seq thres &optional option)
  "Landmarks of seq (Perng, Parker & Leung, 1999): the first-order landmarks (local extrema), smoothed
by MDPP: a pair of consecutive landmarks (x1 y1) (x2 y2) is removed while x2 - x1 < D and their
difference is below thres. option is nil (no D, difference |y2 - y1|), a number (D), a function (a b)
giving the difference, or a list (D criterion), criterion being :absolute (|y2 - y1|), :relative
(2 |y2 - y1| / (|y1| + |y2|), as in Perng et al.) or a function.
Returns (position type value), type -1 for a minimum and 1 for a maximum."
  (multiple-value-bind (d criterion)
      (cond ((null option) (values nil :absolute))
            ((realp option) (values option :absolute))
            ((functionp option) (values nil option))
            ((listp option) (values (first option) (or (second option) :absolute)))
            (t (error "landmarks: option must be nil, a distance, a function or a list (distance criterion), not ~S." option)))
    (let ((diff (cond ((eq criterion :absolute) #'(lambda (a b) (abs (- b a))))
                      ((eq criterion :relative)
                       #'(lambda (a b)
                           (let ((m (+ (abs a) (abs b))))
                             (if (zerop m) 0 (/ (* 2 (abs (- b a))) m)))))
                      ((functionp criterion) criterion)
                      (t (error "landmarks: criterion must be :absolute, :relative or a function, not ~S." criterion))))
          (lm (local-extrema seq)))
      (flet ((removable (a b)
               (and (or (null d) (< (- (first b) (first a)) d))
                    (< (funcall diff (third a) (third b)) thres))))
        (loop for k = (loop for (a b) on lm
                            for i from 0
                            when (and b (removable a b)) return i)
              while k
              do (setf lm (append (subseq lm 0 k) (nthcdr (+ k 2) lm))))
        lm))))

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

(defgeneric signature (seq mode thres &optional option)
  (:documentation
"Signature of a sequence with some threshold <thres> for variation in intensity.
The argument <thres> may be nil or 0 (no threshold for variations in intensity), or an absolute amplitude (in the unit of the values of <seq>): variations smaller than <thres> are ignored.
 Argument <mode> define the algorithm to compute signature which may require the optional argument <option> :
:minflexmax, returns minima, maxima and inflexions: the minima and maxima of :minmax, and an inflexion where a plateau
 which is not an extremum begins and where it ends (a plateau is a stretch where the sequence varies by less than thres);
:minmax, minima and maxima only: the important extrema of :major-extrema, with |a - b|;
:major-extrema, important extrema (Fink & Gandhi, Conference on Systems, Man and Cybernetics, 2007):
 a minimum (maximum) such that the sequence rises (falls) by at least thres on both sides;
:landmarks, first-order landmarks smoothed by MDPP(D, P) (Perng, Parker & Leung, Conference on Information
 and Knowledge Management, 1999): pairs of close landmarks whose difference is below thres are removed.
Any other method is an error.
Argument <thres> is the treshold value (delta) in amplitude.
Argument <option> is an optional parameter depending to the choosen method :
with :minflexmax, <option> is the minimal duration of a plateau, a number of positions (nil, 0, 1 and 2 mean 2);
with :minmax, <option> is not used;
with :major-extrema, <option> may be a distance function (a b) for comparing values (|a - b| by default);
with :landmarks, <option> is nil, a number D (only landmarks closer than D ranks may be removed), a function (a b) giving the
difference between two values (|a - b| by default), or a list (D criterion), criterion being :absolute, :relative
(2 |a - b| / (|a| + |b|), as in Perng et al.) or a function."))

(defmethod signature ((seq null) (mode t) (thres t) &optional (option nil))
  (declare (ignore option))
  nil)

(defmethod signature ((seq list) (mode t) (thres number) &optional (option nil))
  (case mode
    (:minmax (major-extrema seq thres))
    (:minflexmax (minflexmax seq thres option))
    (:major-extrema (major-extrema seq thres
                                   (if (functionp option) option #'(lambda (a b) (abs (- a b))))))
    (:landmarks (landmarks seq thres option))
    (t (error "signature: unknown method ~S, use :minmax, :minflexmax, :major-extrema or :landmarks." mode))))

(defun droite (a b)
  (if (= (cadr a) (cadr b))
      (list 0 (cadr a))
    (let ((pente (/ (- (cadr a) (cadr b)) (- (car a) (car b)))))
      (list pente (- (cadr a) (* pente (car a)))))))

(defun echantillonage (seq method mode thres option)
  "The points of the signature of seq, with its first and last values, according to mode
(:values, :positions, :all or :resampled)."
  (unless (member mode '(:values :positions :all :resampled))
    (error "exsample: unknown mode ~S, use :values, :positions, :all or :resampled." mode))
  (let ((pos (remove-duplicates
              (append (list 0)
                      (mapcar #'car (signature seq method thres option))
                      (list (1- (length seq)))))))
    (case mode
      (:values
       (mapcar #'(lambda (x) (nth x seq)) pos))
      (:positions pos)
      (:all
       (mapcar #'(lambda (x) (list x (nth x seq))) pos))
      (:resampled
       (let ((r (list)))
         (loop for i from 0 to (- (length pos) 2) do
               (let* ((xa (nth i pos))
                      (xb (nth (1+ i) pos))
                      (steps (- xb xa 1))
                      (ya (nth xa seq))
                      (yb (nth xb seq))
                      (d (droite (list xa ya) (list xb yb))))
                 (push ya r)
                 (when (> steps 0)
                   (loop for j from 1 to steps do
                         (push (+ (* (car d) (+ xa j)) (cadr d)) r)))))
         (append (nreverse r) (last seq)))))))

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
;; primitives are contour features - ie signatures or landmarks - (forme of changes of direction) in a sequence of numeric values ;
;; a primitive is made of 3 dots defining 2 segments combining ascending, descending or inflexion contour...

(defun sign (x &optional thres)
  (if (not thres)
      (if (zerop x) 0 (if (plusp x) 1 -1))
    (if (<= (abs x) thres) 0 (if (plusp x) 1 -1))))

(defun primitive-value (triplet &optional thres)
  (let ((a (sign (- (cadr triplet) (car triplet)) thres))
        (b (sign (- (caddr triplet) (cadr triplet)) thres)))
    (if (or (zerop a) (zerop b))  ;; and or or or xor ?
	0 (cond ((and (plusp a) (minusp b)) 1)
		((and (minusp a) (plusp b)) -1)
            (t nil)))))
#|
(defun primitive-value (triplet &optional thres)
  (let ((a (sign (- (cadr triplet) (car triplet)) thres))
        (b (sign (- (caddr triplet) (cadr triplet)) thres)))
    (cond ((and (zerop a) (zerop b)) 0)
          ((and (zerop a) (not (zerop b))) 0)
          ((and (plusp a) (zerop b)) 1)
          ((and (minusp a) (zerop b)) -1)
          ((and (plusp a) (minusp b)) 1)
          ((and (minusp a) (plusp b)) -1)
          (t (print 'x) nil))))
|#

(defun primitive-sign (a b thres) ; a ou b = (pos primitive-value value)
  (if b  ; b may be nil in some cases
      (sign (- (caddr b) (caddr a)) thres)
					;(* (cadr a) (cadr b))		
      (cadr a)))

(defun mfm (seq i)
  (if seq (let ((p (cadar seq)))
            (if (zerop p)
                (mfm (cdr seq) (1+ i))
              i)) i))

;(mfm '((1 0 1) (2 0 2) (3 0 3) (4 0 4)) 0)

(defun primitives-nothres (seq)
  (let ((lm))
    (loop for i from 0 to (- (length seq) 3) do
          (let ((k (primitive-value (subseq seq i (+ i 3)))))
            (when k
              (push (list (1+ i) k (nth (1+ i) seq)) lm))))
    (setf lm (nreverse lm))
    (loop for i from 0 to (- (length lm) 2) do
          (let ((p (nth (1+ i) lm))
                (p-1 (nth i lm))
                (n 0))
            (when (zerop (cadr p))
              (setf n (mfm (subseq lm i) 1))
              (when  (nth (+ i n 1) lm)
                (when (= (cadr p-1) (cadr (nth (+ i n 1) lm)))
                  (setf (cadr p) (* -1 (cadr p-1))))
                (incf i n)))) )
    (values lm)))

(defun primitives-raw (seq thres)
  (if (or (not thres) (zerop thres))
      (primitives-nothres seq)
    (let ((lm (primitives-nothres seq)))
      (loop for i from 0 to (- (length lm) 2) do
            (let ((a (nth i lm))
                  (b (nth (1+ i) lm)))
              (when (zerop (primitive-sign a b thres))
                ;(setf (nth i lm) (list (car a) 0 (caddr a))) )))
                (setf (nth (1+ i) lm) (list (car b) 0 (caddr b))))))
      (values lm))))

(defun notzerop (x)
  (if x (not (zerop x)) nil))

(defun primitives-thres (seq thres &optional (flex-thres 2))
  (let ((lm (primitives-raw seq thres))
        (ft (max flex-thres 2))
        cime r)
    (if (or (not thres) (zerop thres)) lm
      (progn
        (loop until (null lm) do
              (let ((k (pop lm)))
                (if (zerop (cadr k))
                    (progn
                      (setf cime (list k))
                      (loop until (or (notzerop (primitive-sign (car cime) (car lm) thres))
                                      (not lm)) do
                            (push (pop lm) cime))
                      ;(print cime)
                      (if (eq (cadar r) (cadar lm))  ; m f f m > m M m / M f f M > M m M
                          (if (> (length cime) ft)
                              (let* ((vn (cadar lm))
                                     (max (if (plusp vn)
                                              (car (sort cime '< :key #'caddr))
                                            (car (sort cime '> :key #'caddr)))))
                                (setf (cadr max) (* -1 vn))
                                (push max r))
                              (setf r (cdr r)))
                        (if (> (length cime) ft)
                            (let ((mean (float (/ (apply #'+ (mapcar #'caddr cime)) (length cime)))))
                              (mapcar #'(lambda (x) (setf (caddr x) mean)) cime)
                              (setf r (append (list (car cime) (car (last cime))) r)))
                          (let* ((vn (cadar r))
                                 (max (if (plusp vn)
                                          (car (sort cime '> :key #'caddr))
                                        (car (sort cime '< :key #'caddr)))))
                            (if (plusp vn)
                                (when (< (caddar r) (caddr max))
                                  (setf (cadr max) (* 1 vn))
                                    (setf (car r) max))
                              (when (> (caddar r) (caddr max))
                                (setf (cadr max) (* 1 vn))
                                (setf (car r) max))) ))))
                  (push k r))))
        (values (nreverse r))))))

(defun primitives (seq thres &optional (mode t) (inflex-thres 2))
  (let ((lm (primitives-thres seq thres inflex-thres)))
    (if mode lm (remove-if #'(lambda (x) (zerop (cadr x))) lm))))


(defun flat-primitives (seq thres &optional (mode t) (inflex-thres 2))
  "primitives of seq where each run of equal values (plateau) counts as a single point, at its first position:
a flat extremum is given once, as a minimum or a maximum. With mode t (inflexions), a plateau which is
not an extremum gives one inflexion, at its first position."
  (let* ((v (coerce seq 'vector))
         (last (1- (length v)))
         (runs (loop with i = 0
                     while (<= i last)
                     collect (let ((j (loop for k from i
                                            while (and (< k last) (= (aref v (1+ k)) (aref v i)))
                                            finally (return k))))
                               (prog1 (list i (aref v i) (1+ (- j i)))
                                 (setf i (1+ j))))))
         (lm (mapcar #'(lambda (p) (list (first (nth (first p) runs)) (second p) (third p)))
                     (primitives (mapcar #'second runs) thres mode inflex-thres))))
    (when mode
      (dolist (run runs)
        (destructuring-bind (i value n) run
          (when (and (> n 1) (< 0 i last) (not (assoc i lm)))
            (push (list i 0 value) lm)))))
    (sort lm #'< :key #'first)))

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

(defgeneric signature (seq mode thres &optional option)
  (:documentation
"Signature of a sequence with some threshold <thres> for variation in intensity.
The argument <thres> may be nil or 0 (no threshold for variations in intensity), or an absolute amplitude (in the unit of the values of <seq>): variations smaller than <thres> are ignored.
 Argument <mode> define the algorithm to compute signature which may require the optional argument <option> :
:minflexmax, returns minima, maxima and inflexions using Fred Voisin pragmatic method.
:minmax, same with only maxima and minima (without inflexions);
:major-extrema, important extrema (Fink & Gandhi, Conference on Systems, Man and Cybernetics, 2007):
 a minimum (maximum) such that the sequence rises (falls) by at least thres on both sides;
:landmarks, first-order landmarks smoothed by MDPP(D, P) (Perng, Parker & Leung, Conference on Information
 and Knowledge Management, 1999): pairs of close landmarks whose difference is below thres are removed.
Any other method is an error.
Argument <thres> is the treshold value (delta) in amplitude.
Argument <option> is an optional parameter depending to the choosen method :
with :minflexmax and :minmax, <option> is used only when thres > 0: the local extrema which differ from the previous one
by thres or less are the turns of a slightly varying plateau, and <option> is the number of turns a plateau must exceed to be kept
(nil, 0, 1 and 2 count as 2); a kept plateau gives two inflexions, at its first and last turns, valued at the mean of its turns;
a shorter plateau is absorbed in the preceding extremum. A non-integer <option> becomes floor((max - min) * option * length);
with :major-extrema, <option> may be a distance function (a b) for comparing values (|a - b| by default);
with :landmarks, <option> is nil, a number D (only landmarks closer than D ranks may be removed), a function (a b) giving the
difference between two values (|a - b| by default), or a list (D criterion), criterion being :absolute, :relative
(2 |a - b| / (|a| + |b|), as in Perng et al.) or a function."))

(defmethod signature ((seq null) (mode t) (thres t) &optional (option nil))
  (declare (ignore option))
  nil)

(defmethod signature ((seq list) (mode t) (thres number) &optional (option nil))
  (let* ((max (lmax seq))
         (min (lmin seq))
         (thres1 (if thres thres 0.0))
         (thres2 (if (integerp option) option (floor (* (- max min) (if (numberp option) option 0) (length seq)))))) ;) (* y x th)
    (case mode
      (:minmax (flat-primitives seq thres1 nil thres2))
      (:minflexmax (flat-primitives seq thres1 t thres2))
      (:major-extrema (major-extrema seq thres1
                                     (if (functionp option) option #'(lambda (a b) (abs (- a b))))))
      (:landmarks (landmarks seq thres1 option))
      (t (error "signature: unknown method ~S, use :minmax, :minflexmax, :major-extrema or :landmarks." mode)))))

(defmethod signature ((seq list) (mode t) (thres null) &optional (option nil))
  (signature seq mode 0 option))

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

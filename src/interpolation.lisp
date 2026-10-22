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

;;;; interpolation.lisp
;;;;
;;;; Interpolation of two sequences of symbols (Levenshtein).

(in-package :fv-morphologie)

;; s-interpolate.lisp
;; interpolate two lists as sequences of symbols using some levenshtein distance algorithm.
;; Fred Voisin, pour Julien Vincenot, 2013.

(defun s2l (string)
 (let ((r nil))
    (dotimes (n (length string) (nreverse r))
      (when (not (eql #\ (elt string n)))
	(push (safe-read-from-string (string (elt string n))) r)))))

(defun m-levenshtein (s1 s2 &key (test #'eql))
 (let* ((width (1+ (length s1)))
	 (height (1+ (length s2)))
	 (d (make-array (list height width)
                       :element-type 'fixnum))
        (s1 (coerce s1 'vector))
        (s2 (coerce s2 'vector)))
   (dotimes (x width)
     (setf (aref d 0 x) x))
   (dotimes (y height)
     (setf (aref d y 0) y))
   (dotimes (x (length s1))
     (dotimes (y (length s2))
       (let ((cost (if (funcall test (aref s1 x) (aref s2 y)) 0 1)))
         (setf (aref d (1+ y) (1+ x))
               (min (1+ (aref d y (1+ x)))
                    (1+ (aref d (1+ y) x))
                    (+ (aref d y x)
                       cost))))))
   (values d)))

(defun ins (a b x y)
  (let ((a1 (subseq a 0 (1+ x)))
        (a2 (subseq a (1+ x))))
    (append a1 (list (nth y b)) a2)))

(defun sup (a b x y)
  (declare (ignore b) (ignore y))
  (append (subseq a 0 x) (subseq a (1+ x))))

(defun sub (a b x y)
  (let ((a1 (subseq a 0 x))
        (a2 (subseq a (1+ x))))
    (append a1 (list (nth y b)) a2)))

(defun s-interpolate (a b &key (test 'eql))
  (let ((m (m-levenshtein b a :test test))  ; matrice
        (p (list (list (length b) (length a)))) ; positions
        (r (list b)) ; resultat
        (o nil)) ; operation
    (loop until (eval `(and .,(mapcar #'zerop (car p))))
          do
          (let* ((x (caar p))
                 (y (cadar p))
                 (np nil)
                 (coutsvoisins (if (zerop x)
                                   (if (zerop y)
                                       (abort)
                                     (list (list x (1- y) (aref m (- y 1) x))))
                                 (if (zerop y)
                                     (list (list (1- x) y (aref m y (- x 1))))
                                   (list (list (1- x) (1- y) (aref m (- y 1) (- x 1)))
                                         (list (1- x) y (aref m y (- x 1)))
                                         (list x (1- y) (aref m (- y 1) x))))))
                 (min (apply #'min (mapcar #'third coutsvoisins))))
            (setf coutsvoisins (remove-if-not #'(lambda (x) (= min (caddr x))) coutsvoisins))
            (if (= 1 (length coutsvoisins))
                (setf np (car coutsvoisins))
              (setf np (nth (random (length coutsvoisins)) coutsvoisins)))
            (setf o (if (= x (car np))
                        (if (= y (cadr np))
                            (abort)
                          (setf o #'ins))
                      (if (= y (cadr np))
                          (setf o #'sup)
                        (setf o #'sub))))
	    (push (funcall o (car r) a (1- x) (1- y)) r)
            (push (butlast np) p)))
    (values r)))

;(s-interpolate '(a m p h o r a) '(p o u l e t))

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

;;;; information.lisp
;;;;
;;;; Statistics and information: histograms, Shannon entropy, conditional
;;;; entropy, self-information.

(in-package :fv-morphologie)

;;;
;; stats

(defun histogramme (data &key (test #'equalp) (thes nil))
  "The list of (element count) of data, for the different elements (or those of thes): sorted by
increasing value for numbers, else by decreasing count, elements of equal count in their order of
first appearance (in data, or in thes)."
  (let* ((cl (or thes (remove-duplicates data :test test :from-end t)))
         (histo (mapcar #'(lambda (c) (list c (count-if #'(lambda (x) (funcall test c x)) data)))
                        cl)))
    (if (every #'numberp cl)
        (stable-sort histo #'< :key #'car)
        (stable-sort histo #'> :key #'cadr))))

;;;
;; info theory

(defparameter *e* 2.7182818284590452353602874) ; (/ 848456353 312129649) = (exp 1)

#-Lispworks
(defun e-shannon (data &optional (base nil) (test #'equalp))
  "Returns the shannon entropy value of data.
Shannon entropy value may be between 0 and (log n base)."
  (let* ((cl (remove-duplicates data :test test))
	 (n (length data))
	 (p nil)
	 (b (if (numberp base)
		base (if (eql base 'e)
			 (exp 1)
			 (max 2 (length cl))))))
    (dolist (ci cl (- 0 (apply #'+ (mapcar #'(lambda (x) (* x (log! x b))) p))))
      (push (/ (length (remove-if-not #'(lambda (x) (funcall test x ci)) data)) n)
            p))))

#+Lispworks
(defun e-shannon (data &optional (base nil) (test #'equalp))
  "Returns the shannon entropy value of data.
Shannon entropy value may be between 0 and (log n base)."
  (let* ((cl (remove-duplicates data :test test))
	 (n (length data))
	 (p nil)
	 (b (if (numberp base)
		base (if (eql base 'e)
			 (exp 1)
			 (max 2 (length cl))))))
    (dolist (ci cl (- 0 (sum-loop(mapcar #'(lambda (x) (* x (log! x b))) p))))
      (push (/ (length (remove-if-not #'(lambda (x) (funcall test x ci)) data)) n)
            p))))

(defun windowed-e-shannon (data n &optional (base 2) (test #'equalp))
  "Applique e-shannon sur fenetres de taille n echantillons"
  (let* ((L (length data))
	 (w (/ L n)) )
    (loop for i from 0 to (1- n) ;(floor (/ L step)))
       collect (let ((k (floor (* (1+ i) w))))
		 (e-shannon (subseq data (floor (* i w)) (if (< k L) k L))
			    base test)))))

(defun shannon-entropy (data &optional (base nil) (samples nil) (test #'equalp))
  "Returns the entropy value of the data according to Shannon definition."
  (assert (or (integerp samples) (null samples)))
  (if (not samples)
      (e-shannon data base test)
      (windowed-e-shannon data samples base test)))

#-Lispworks
(defun kind-of-entropie-conditionnelle (data &optional (base 2) (test #'equalp))
  "Kind of conditionnal entropy based on a hack of shannon entropy by me. Experimental."
  (let* ((p nil) (len (length data))
	 (s (remove-duplicates data :test #'equalp)) ;test))
	 (n (length s))
	 (b (if (numberp base)
		base (if (eql base 'e) (exp 1) (max 2 n))))
	 (r (mapcar (lambda (x) (cons x (mapcar (lambda (y) (list y 0)) s))) s)))
    (dotimes (d (1- (length data))) ;build table of P(a if b)
      (let ((k (assoc (nth d data) r :test test))
	    (l (nth (1+ d) data)))
	(incf (cadr (assoc l (cdr k) :test test)) )))
    (dolist (i r (- 0 (apply #'+ (mapcar #'(lambda (x) (* x (log! x (* b b)))) p)))) ; sqr log to scale
      (dolist (j (cdr i))
	(push (/ (cadr j) len) p))))) ; P(i)

#+Lispworks
(defun kind-of-entropie-conditionnelle (data &optional (base 2) (test #'equalp))
  "Kind of conditionnal entropy based on a hack of shannon entropy by me. Experimental."
  (let* ((p nil) (len (length data))
	 (s (remove-duplicates data :test #'equalp)) ;test))
	 (n (length s))
	 (b (if (numberp base)
		base (if (eql base 'e) (exp 1) (max 2 n))))
	 (r (mapcar (lambda (x) (cons x (mapcar (lambda (y) (list y 0)) s))) s)))
    (dotimes (d (1- (length data))) ;build table of P(a if b)
      (let ((k (assoc (nth d data) r :test test))
	    (l (nth (1+ d) data)))
	(incf (cadr (assoc l (cdr k) :test test)) )))
    (dolist (i r (- 0 (sum-loop (mapcar #'(lambda (x) (* x (log! x (* b b)))) p)))) ; sqr log to scale
      (dolist (j (cdr i))
	(push (/ (cadr j) len) p))))) ; P(i)

;(kind-of-entropie-conditionnelle (loop for i from  0 to 1000 collect (random 2)) nil)

(defun windowed-entropie-cond (data n &optional (base nil) (test #'equalp))
  "Applique e-shannon sur fenetres de taille n echantillons."
  (let* ((L (length data))
	 (w (/ L n)) )
    (loop for i from 0 to (1- n) ;(floor (/ L step)))
       collect (let ((k (floor (* (1+ i) w))))
		 (kind-of-entropie-conditionnelle (subseq data (floor (* i w)) (if (< k L) k L))
						  base test)))))

(defgeneric entropie-conditionnelle (data &optional base samples test)
  (:documentation "Kinf of conditionnal entropy based on Shannon entropy (hack)."))

(defmethod entropie-conditionnelle ((data list) &optional (base nil) (samples nil) (test #'equalp))
  (if samples
      (windowed-entropie-cond data (floor (/ (length data) samples)) base test)
      (kind-of-entropie-conditionnelle data base test)))


(defgeneric self-info (data elt &optional test)
  (:documentation "Amount of information of each element <elt> in data: -log2 of its frequency, nil if
it is absent from data (the information is infinite) or if data is empty.
Element <elt> may be of any kind (symbol, number, string, list...).
If <elt> is null, self-info returns the amount of information for each element in data,
sorted in decreasing order of amount of information in data (equal amounts in the order of first appearance)."))

(defmethod self-info ((data list) (elt t) &optional (test #'equalp))
  (when (not test) (setf test #'equalp))
  (let ((n (count-if #'(lambda (x) (funcall test x elt)) data)))
    (if (zerop n)
        nil
        (- 0 (log (/ n (length data)) 2)))))

(defmethod self-info ((data list) (elt null) &optional (test #'equalp))
  (when (not test) (setf test #'equalp))
  (let ((elts (remove-duplicates data :test test :from-end t)))
    (stable-sort (mapcar #'(lambda (x) (list x (self-info data x test))) elts) '> :key #'second)))

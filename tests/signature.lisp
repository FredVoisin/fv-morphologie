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

(in-package :fv-morphologie/tests)

(def-suite signature :in fv-morphologie
  :description "2. Classification: signatures, sampling and split.")
(in-suite signature)

(test split
  (is (equal '("hello" "big" "world") (split "hello big world")))
  (is (equal '((a b) (c d) (e)) (split '(a b x c d x e) '(x)))))

(test split-list-uses-marks
  (is (equal '((a b) (c)) (split '(a b x c) '(x)))))

(test exsample-fixes
  (let ((s '(0 2 4 3 1 2 6 5 0)))
    (is (equal '(0 4 1 6 0) (exsample s :minmax :values 0)))
    (is (equal '(0 2 4 5/2 1 7/2 6 3 0) (exsample s :minmax :resampled 0)))
    ;; nil method or mode: the defaults (mode nil used to give nil)
    (is (equal '(0 4 1 6 0) (exsample s nil :values 0)))
    (is (equal '(0 4 1 6 0) (exsample s :minmax nil 0)))
    ;; a single value used to be doubled
    (is (equal '(3) (exsample '(3) :minmax :resampled 0)))
    (is (equal '(0) (exsample '(3) :minmax :positions 0)))
    ;; :resampled keeps the length
    (is (= 9 (length (exsample s :minflexmax :resampled 0))))
    ;; unknown modes and methods used to give nil or :minflexmax silently
    (signals error (exsample s :minmax :foo 0))
    (signals error (exsample s :foo :values 0))
    ;; :resampled keeps the length with all the methods
    (dolist (m '(:minmax :minflexmax :major-extrema :landmarks))
      (is (= 9 (length (exsample s m :resampled 0)))))))

(test signature-major-extrema
  ;; Fink & Gandhi important extrema (the name used to give :minflexmax); thres is absolute
  (let ((r '(0 1 3 2 2.5 1 4 8 7 7.5 3 2 5 1 0)))
    ;; thres 0: every strict local extremum
    (is (equal (int-signature r :minmax 0) (int-signature r :major-extrema 0)))
    (is (equal '((7 1 8) (11 -1 2) (12 1 5)) (int-signature r :major-extrema 2.4)))
    (is (equal '((7 1 8)) (int-signature r :major-extrema 4.8)))
    ;; absolute: scaling the data needs a scaled threshold
    (is (equal (int-signature r :major-extrema 2.4)
               (mapcar #'(lambda (p) (list (first p) (second p) (/ (third p) 10)))
                       (int-signature (mapcar #'(lambda (x) (* 10 x)) r) :major-extrema 24))))
    ;; extrema alternate
    (is (loop for (a b) on (int-signature r :major-extrema 0.8) while b
              always (= (second a) (- (second b)))))
    ;; a distance function as option
    (is (equal (int-signature r :major-extrema 2.4)
               (int-signature r :major-extrema 2.4 #'(lambda (a b) (abs (- a b))))))
    (is (equal '((1 -1 0)) (int-signature '(9 0 0 0 9) :major-extrema 0)))
    (is (null (int-signature '(1 2 3 4) :major-extrema 0)))
    (is (null (int-signature '(5) :major-extrema 0)))
    (is (null (int-signature '() :major-extrema 0)))
    (is (equal '(0 8 2 5 0) (exsample r :major-extrema :values 2.4)))))

(test signature-landmarks
  ;; Perng, Parker & Leung landmarks with MDPP smoothing (the name used to give :minflexmax)
  (let ((r '(0 1 3 2 2.5 1 4 8 7 7.5 3 2 5 1 0))
        (absolute '((2 1 3) (5 -1 1) (7 1 8) (11 -1 2) (12 1 5)))
        (relative '((2 1 3) (5 -1 1) (7 1 8) (11 -1 2) (12 1 5))))
    ;; thres 0: the first-order landmarks, i.e. the local extrema
    (is (equal (int-signature r :minmax 0) (int-signature r :landmarks 0)))
    ;; absolute difference (default): the pairs (2 2.5) and (7 7.5) are removed
    (is (equal absolute (int-signature r :landmarks 0.6)))
    (is (equal absolute (int-signature r :landmarks 0.6 '(nil :absolute))))
    ;; the relative difference of Perng et al.: the pairs (2 2.5) and (7 7.5) are removed (smallest first)
    (is (equal relative (int-signature r :landmarks 0.3 '(nil :relative))))
    ;; a difference function, alone or with D
    (is (equal absolute (int-signature r :landmarks 0.6 #'(lambda (a b) (abs (- a b))))))
    (is (equal absolute (int-signature r :landmarks 0.6 (list nil #'(lambda (a b) (abs (- a b)))))))
    ;; D = 1: no pair is close enough to be removed; D = 2: both pairs are
    (is (equal (int-signature r :landmarks 0) (int-signature r :landmarks 0.6 1)))
    (is (equal absolute (int-signature r :landmarks 0.6 2)))
    (is (equal relative (int-signature r :landmarks 0.3 '(2 :relative))))
    (signals error (int-signature r :landmarks 1 '(nil :foo)))
    (is (equal '((1 -1 0)) (int-signature '(9 0 0 0 9) :landmarks 0)))
    (is (null (int-signature '(1 2 3 4) :landmarks 0)))
    (is (null (int-signature '() :landmarks 0)))
    (is (= 15 (length (exsample r :landmarks :resampled 0.6))))))

(test landmarks-order-and-distance
  ;; the pair of smallest difference first: 4, 5 goes before 6, 4
  (is (equal '((1 1 6)) (int-signature '(0 6 4 5 0) :landmarks 3)))
  ;; the left-most of equal differences, then the landmarks before and after the pair are consecutive
  (is (equal '((5 1 6)) (int-signature '(0 4 3 5 4 6 0) :landmarks 2)))
  (is (equal '((1 1 9) (6 -1 0)) (int-signature '(5 9 5 6 5 6 0 4) :landmarks 2)))
  ;; D is nil or a non-negative number
  (signals error (int-signature '(0 3 1 2 0) :landmarks 1 -2))
  (signals error (int-signature '(0 3 1 2 0) :landmarks 1 '(:foo)))
  ;; a long sequence (removing the pairs was quadratic)
  (let ((s (loop for i below 100000 collect (if (evenp i) 0 1))))
    (is (null (int-signature s :landmarks 2)))))

(test signature-flat-extrema
  ;; a flat extremum is given once, at its first position, by all the methods
  ;; (:minmax ignored plateaus, :minflexmax marked each of their points as an inflexion)
  (dolist (m '(:minmax :minflexmax :major-extrema :landmarks))
    (is (equal '((1 -1 0)) (int-signature '(9 0 0 0 9) m 0)))
    (is (equal '((1 1 5)) (int-signature '(0 5 5 5 5 0) m 0)))
    ;; extrema alternate (:minmax gave two minima in a row)
    (is (equal '((1 1 4) (2 -1 0) (3 1 4) (5 -1 0)) (int-signature '(0 4 0 4 4 0 4) m 0))))
  ;; :minflexmax: a plateau which is not an extremum gives an inflexion where it begins and where it ends
  (is (equal '((2 0 0) (3 0 5)) (int-signature '(0 0 0 5 5 5) :minflexmax 0)))
  (is (equal '((1 0 3) (2 0 3) (3 1 6)) (int-signature '(0 3 3 6 6 0) :minflexmax 0)))
  (is (null (int-signature '(0 0 0 5 5 5) :minmax 0)))
  ;; sequences without plateaus are unchanged
  (is (equal '((2 1 4) (4 -1 1) (6 1 6)) (int-signature '(0 2 4 3 1 2 6 5 0) :minmax 0))))

(test signature-thres-nil
  ;; thres nil (documented: no threshold) found no applicable method in int-signature and exsample,
  ;; and (zerop nil) failed in primitives-raw and primitives-thres
  (let ((s '(0 2 4 3 1 2 6 5 0)))
    (dolist (m '(:minmax :minflexmax :major-extrema :landmarks))
      (is (equal (int-signature s m 0) (int-signature s m nil))))
    (is (equal (exsample s :minmax :values 0) (exsample s :minmax :values nil)))))

(test split-fixes
  ;; lists: marks used to be compared with eql (string or list marks never matched)
  (is (equal '(("a") ("b")) (split '("a" "," "b") '(","))))
  (is (equal '(((a) (b)) ((c))) (split '((a) (b) (0) (c)) '((0)))))
  ;; a single mark used to return the list unsplit
  (is (equal '((a) (b)) (split '(a x b) 'x)))
  (is (equal '((a b) (c)) (split '(a b x x c x) '(x))))
  (is (equal '((2 3 4) (2)) (split '(1 2 3 4 1 2) (lambda (x) (< x 2)))))
  (is (equal '((a b c)) (split '(a b c))))
  (is (null (split '(x x) '(x))))
  ;; strings: the result is always a list ("hello" gave "hello", "" gave "")
  (is (equal '("hello") (split "hello")))
  (is (null (split "")))
  (is (null (split "   ")))
  (is (equal '("a" "b") (split "a b" nil)))
  (is (equal '("a" "b" "c") (split "a,b;c" ",;")))
  (is (equal '("été" "à") (split "été,à" ","))))

(test signature-minflexmax-plateaus
  ;; a plateau is a stretch of at least option positions (2 by default) where the sequence varies by
  ;; less than thres; it gives an inflexion where it begins and where it ends, valued as in the sequence
  (let ((s '(0 2 4 5 5.2 5 5.2 5 7 9)))  ; a plateau of 5 positions (3 to 7) in a rise
    (dolist (o '(nil 0 2 5))
      (is (equal '((3 0 5) (7 0 5)) (int-signature s :minflexmax 1 o))))
    (is (null (int-signature s :minflexmax 1 6)))
    ;; the small variations of the plateau give no extremum (the first one gave a maximum)
    (is (null (int-signature s :minmax 1)))
    ;; a variation of thres or more is not a plateau
    (is (null (int-signature '(0 2 4 5 6 7 9) :minflexmax 1))))
  ;; a plateau which contains an extremum is a flat extremum, given once
  (is (equal '((1 1 6) (2 -1 3) (7 1 6)) (int-signature '(0 6 3 3.2 3 3.2 3 6 0) :minflexmax 1)))
  (is (equal '((4 1 8.6)) (int-signature '(0 8 8.5 8.2 8.6 0) :minflexmax 1)))
  ;; a slow drift is not a succession of small extrema (each dip of 0.2 gave a maximum and a minimum)
  (is (equal '((13 1 8)) (int-signature '(0 3 3.5 3.3 3.8 3.6 4.1 3.9 4.4 4.2 4.7 4.5 5 8 0) :minmax 1)))
  ;; option is a number of positions
  (signals error (int-signature '(0 1 0) :minflexmax 1 0.5))
  (signals error (int-signature '(0 1 0) :minflexmax 1 -1)))

(test major-extrema-ends-ties-and-termination
  ;; Fink & Gandhi relax the definition at the ends: the minimum 0 needs no rise of 5 towards the end
  (is (equal '((1 1 8) (3 -1 0)) (int-signature '(0 8 1 0 2) :major-extrema 5)))
  ;; but some variation of at least thres is needed
  (is (null (int-signature '(8 4 5) :major-extrema 5)))
  ;; of equal extrema, the first; an extremum equal to the first value is that value
  (is (equal '((2 -1 1) (3 1 8) (9 -1 1) (13 1 8)) (int-signature '(5 2 1 8 5 6 6 8 6 1 2 5 1 8 7 2) :major-extrema 6)))
  (is (equal '((3 1 8) (4 -1 1)) (int-signature '(2 4 2 8 1 4) :major-extrema 4)))
  ;; a negative threshold never ended (and :landmarks took it as 0)
  (dolist (m '(:minmax :minflexmax :major-extrema :landmarks))
    (signals error (int-signature '(1 2 3 1) m -1)))
  (signals error (exsample '(1 2 3 1) :minflexmax :values -1))
  ;; nor a distance which is not 0 between equal values
  (is (listp (int-signature '(1 2 3 1) :major-extrema 0.5 #'(lambda (a b) (declare (ignore a b)) 1)))))

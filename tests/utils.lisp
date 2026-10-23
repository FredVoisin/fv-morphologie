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

(def-suite utils :in fv-morphologie
  :description "Utilities: lists and maths.")
(in-suite utils)

(test explicit-errors-on-empty-lists
  ;; lmin/lmax failed in (random 0), fmax/fmin returned nil, mean and normalize divided by zero
  (dolist (f '(fv-morphologie::lmin fv-morphologie::lmax fv-morphologie::fmax
               fv-morphologie::fmin fv-morphologie::mean))
    (is (search "empty" (handler-case (progn (funcall f nil) "") (error (e) (princ-to-string e))))))
  (signals error (fv-morphologie::normalize '(1 -1)))
  (signals error (fv-morphologie::normalize '(0 0)))
  (is (null (fv-morphologie::normalize '())))
  (is (equal '(1/4 3/4) (fv-morphologie::normalize '(1 3))))
  ;; the values are unchanged
  (is (= 1 (fv-morphologie::lmin '(3 1 2))))
  (is (= 3 (fv-morphologie::lmax '(3 1 2))))
  (is (= 3 (fv-morphologie::fmax '(3 1 2))))
  (is (= 1 (fv-morphologie::fmin '(3 1 2))))
  (is (approx= 2 (fv-morphologie::mean '(1 2 3)))))

(test derivee
  ;; signs of the successive differences, as the reference loop on nth
  (flet ((reference (seq)
           (loop for i from 0 below (1- (length seq))
                 for a = (nth i seq)
                 for b = (nth (1+ i) seq)
                 collect (cond ((> b a) 1) ((< b a) -1) (t 0)))))
    (is (equal '(1 1 0 1 1 -1 -1 1 1) (fv-morphologie::derivee '(1 2 3 3 5 6 5 3 8 9))))
    (is (null (fv-morphologie::derivee '())))
    (is (null (fv-morphologie::derivee '(4))))
    (is (equal '(-1 0 1) (fv-morphologie::derivee '(2.5 1 1 3/2))))
    (let ((*random-state* (seeded-random-state)))
      (dotimes (k 50)
        (let ((seq (loop repeat (random 30) collect (- (random 7) 3))))
          (is (equal (reference seq) (fv-morphologie::derivee seq))))))))

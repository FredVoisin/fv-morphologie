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

(def-suite marks :in fv-morphologie
  :description "Marks: positions, lists, structures, inner dynamic.")
(in-suite marks)

(test mark-position
  (is (equal '(0 3) (mark-position '(a b c a b) 'a))))

(test mark-list
  (is (equal '(((a b c) (0)) ((a b d) (3)))
             (mark-list '(a b c a b d) 'a))))

(test mark-structure
  (is (equal '((0 1 2) (0 1 2)) (mark-structure '(a b c a b d a b) nil))))

(test mark-structure-fixes
  (let ((close (lambda (x y) (< (abs (- x y)) 1))))
    ;; segment grouping
    (is (equal '((0 1 0 2 1)) (remove-if-not #'(lambda (s) (= 5 (length s)))
                                             (mark-structure '(a 1 a 2 a 1 a 3 a 2) nil))))
    ;; :raw gives each different segment followed by all its positions
    (is (equal '((0 0 0) (((a b c) 0 3 6)))
               (find '(0 0 0) (mark-structure '(a b c a b c a b c) :raw) :key #'car :test #'equal)))
    ;; :rem-loc-dup inside segments, positions in the original sequence
    (is (equal '((0 0) ((0 3) (a b) (a b)))
               (find '(0 0) (mark-structure '(a a b a b) :pos) :key #'car :test #'equal)))
    ;; :rem-loc-dup uses :test (it used equalp)
    (is (equal '((0 1) ((0 4) (1 2 9) (1 2 7)))
               (first (mark-structure '(1 1.5 2 9 1 2 2.2 7) :pos :test close))))
    (is (equal '((0 1) ((0 4) (1 1.5 2 9) (1 2 2.2 7)))
               (first (mark-structure '(1 1.5 2 9 1 2 2.2 7) :pos :test close :rem-loc-dup nil))))
    ;; an unknown output returned nil silently
    (signals error (mark-structure '(a b a) :foo))
    (is (equal (mark-structure '(a b c a b d a b) :struct) (mark-structure '(a b c a b d a b) nil)))))

(test inner-dynamic-bounds
  ;; the bounds of the sequence were the symbols *xstart* and *xstop*: the same symbols
  ;; in the sequence were mistaken for them
  (is (equal '(12 7 6) (inner-dynamic '(a z a))))
  (is (equal '(12 7 6) (inner-dynamic (list 'a 'fv-morphologie::*xstart* 'a))))
  (is (equal '(12 7 6) (inner-dynamic (list 'a 'fv-morphologie::*xstop* 'a))))
  ;; one value per element; the documented example
  (is (equal '(34 24 25 125 17 145 10 154) (inner-dynamic '(a b a c a d a e))))
  (is (null (inner-dynamic '()))))

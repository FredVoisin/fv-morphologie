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

(def-suite information :in fv-morphologie
  :description "3. Evaluation: information.")
(in-suite information)

(test histogram
  (is (equal '((a 3) (b 2) (c 1)) (histogram '(a b a c a b)))))

(test entropy
  ;; entropy prints its mode to standard output
  (let ((*standard-output* (make-broadcast-stream)))
    (is (approx= 0.0 (entropy '(a a a a))))
    (is (approx= 1.0 (entropy '(a b a b))))
    (is (approx= 2.0 (entropy '(a b c d))))))

(test elt-info
  (is (approx= 1.0 (elt-info '(a b a c) 'a))))

(test histogram-test-and-order
  ;; the default test was eq: equal strings or lists were not counted together
  (is (equal '(("a" 2) ("b" 1)) (histogram '("a" "b" "a"))))
  (is (equal '(((1 2) 2) ((3) 1)) (histogram '((1 2) (3) (1 2)))))
  (is (equal '(("The" 2) ("cat" 1)) (histogram '("The" "cat" "the"))))
  (is (equal '(("The" 1) ("cat" 1) ("the" 1)) (histogram '("The" "cat" "the") :test #'equal)))
  ;; equal counts in the order of first appearance (it was arbitrary)
  (is (equal '((b 2) (a 2) (c 1)) (histogram '(b a c a b))))
  (is (equal '((c 1) (b 1) (a 1)) (histogram '(c b a))))
  (is (equal '((b 1) (a 1) (c 0)) (histogram '(a b) :thes '(c b a))))
  ;; numbers by increasing value
  (is (equal '((1 2) (2 1) (3 3)) (histogram '(3 1 2 1 3 3))))
  (is (null (histogram '()))))

(test entropy-modes
  ;; modes were compared with eq to the symbols of fv-morphologie: from another package,
  ;; 'shannon-2 or :shannon-2 printed "unknown mode" and returned nil
  (let ((*package* (find-package :cl-user)))
    (is (approx= 2 (entropy '(a b c d) (intern "SHANNON-2" :cl-user)))))
  (is (approx= 2 (entropy '(a b c d) :shannon-2)))
  (is (approx= 0.9463947 (entropy '(a b a c) "shannon-n")))
  (is (approx= (log 2) (entropy '(a b) :shannon-e)))
  (is (approx= 0.5141604 (entropy '(a b a b a b) :cond-sh)))
  ;; the default mode printed a message at each call
  (is (equal "" (with-output-to-string (*standard-output*) (entropy '(a b a b)))))
  ;; an unknown mode printed a message and returned nil
  (signals error (entropy '(a b c) :foo))
  ;; always floats (0 and lists mixing 0 and 0.0)
  (is (eql 0.0 (entropy '() :shannon-2)))
  (is (every #'floatp (entropy '(a b) :shannon-2 5)))
  (is (equal '(1.0 1.0 0.0 0.0) (entropy '(a b a b a a a a) :shannon-2 4))))

(test elt-info-any-element
  ;; an absent element, or empty data, divided by zero: nil (the information is infinite)
  (is (null (elt-info '(a b c) 'z)))
  (is (null (elt-info '() 'a)))
  (is (null (elt-info '() nil)))
  ;; strings, decimal numbers and lists had no applicable method
  (is (approx= (- (log 2/3 2)) (elt-info '("x" "y" "x") "x")))
  (is (approx= 1 (elt-info '(1.5 2.5 1.5 3.5) 1.5)))
  (is (approx= (- (log 2/3 2)) (elt-info '((1 2) (3) (1 2)) '(1 2))))
  ;; nil: every element, with test, equal amounts in the order of first appearance
  (is (equal '((b 2.0) (c 2.0) (a 1.0)) (elt-info '(a b a c) nil)))
  (is (equal '("The" "the" "cat") (mapcar #'first (elt-info '("The" "the" "cat") nil #'equal))))
  (is (approx= 0 (elt-info '(a a a) 'a))))

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

(def-suite motifs :in fv-morphologie
  :description "Motifs: find, list, structure, groups.")
(in-suite motifs)

(test motif-find
  (is (equal '((0 1) (3 4) (6 7)) (motif-find '(a b) '(a b c a b d a b))))
  (is (equal '((0 1) (6 7)) (motif-find '(a b) '(a b c a c d a b))))
  (is (equal '((0 1) (3 4) (6 7)) (motif-find '(a b) '(a b c a c d a b) :diss 0.5))))

(test motif-list
  (is (equal '(((a b c) (0 2) (3 5) (7 9)))
             (motif-list '(a b c a b c d a b c) nil))))

(test motif-group
  (is (equal '((1 1) (2 2 2) 3) (motif-group '(1 1 2 2 2 3) #'=))))

(test motif-find-uses-edit-costs
  ;; used to call find-pos with too many arguments
  (finishes (motif-find '(a b) '(a b c) :change 2 :ins 1 :del 1)))

(test motif-list-default-output
  ;; (set out :length) used to fail at compile time
  (finishes (motif-list '(a b a b) nil)))

(test motif-structure-long
  ;; 240 elements took about 6 minutes (edit distance in cubic time, a distance per window)
  (let* ((*random-state* (seeded-random-state))
         (s (loop repeat 240 collect (nth (random 4) '(a b c d))))
         (r (motif-structure s)))
    (is (plusp (length (second r))))
    ;; each motif occurs at least twice
    (is (every #'(lambda (k) (>= (count k (mapcan #'(lambda (p) (copy-list (cdr p))) (first r))) 2))
               (loop for k below (length (second r)) collect k))))
  (is (equal '(((0 0) (3 0)) ((a b c))) (motif-structure '(a b c a b c d)))))

(test motif-structure-keywords
  ;; defaults: motifs of 3 elements or more, exact repetitions
  (is (equal '(nil nil) (motif-structure '(a b a b a b))))
  ;; :lengths
  (is (equal '(((1 0) (4 0) (7 0)) ((a b))) (motif-structure '(x a b y a b z a b) :lengths '(2 2))))
  (is (equal (motif-structure '(x a b y a b z a b) :lengths '(2 2))
             (motif-structure '(x a b y a b z a b) :lengths 2)))
  ;; :test, element identity
  (let ((close (lambda (a b) (<= (abs (- a b)) 1)))
        (s '(60 62 64 70 61 62 65 72 60 63 64)))
    (is (equal '(nil nil) (motif-structure s :lengths '(3 3))))
    ;; one motif: its repetitions are not listed as motifs of their own
    (is (equal '(((0 0) (4 0) (8 0)) ((60 62 64))) (motif-structure s :lengths '(3 3) :test close))))
  ;; :diss, normalized edit distance
  (is (equal '(((0 0) (8 0)) ((a b c))) (motif-structure '(a b c x a b d y a b c) :lengths '(3 3))))
  (is (equal '(((0 1) (3 0) (4 1) (7 0) (8 1)) ((x a b) (a b c)))
             (motif-structure '(a b c x a b d y a b c) :lengths '(3 3) :diss 0.34)))
  ;; motif-list too: (a b d) is a repetition of (a b c), not another motif
  (is (equal '(((x a b) (3 5) (7 9)) ((a b c) (0 2) (4 6) (8 10)))
             (motif-list '(a b c x a b d y a b c) :length :diss 0.34)))
  ;; :distance on whole segments: same total interval
  (flet ((span (s) (- (car (last s)) (first s))))
    (is (equal '(((0 0) (4 0) (8 0)) ((1 2 3)))
               (motif-structure '(1 2 3 9 2 3 4 9 5 6 7) :lengths '(3 3)
                                :distance #'(lambda (a b) (abs (- (span a) (span b))))))))
  (is (equal '(((0 2) (1 1) (2 0) (3 2) (4 1) (5 0)) ((c a b) (b c a) (a b c)))
             (motif-structure '(a b c a b c a b) :criteria :freq)))
  (signals error (motif-structure '(a b c) :criteria :foo))
  (signals error (motif-structure '(a b c) :lengths '(3 2))))

(test motif-list-fixes
  ;; an unknown output returned the motifs unsorted silently
  (signals error (motif-list '(a b c a b c d a b c) :foo))
  (is (equal (motif-list '(a b c a b c d a b c) nil) (motif-list '(a b c a b c d a b c) :length)))
  ;; motifs of 3 elements or more by default; :n a maximum length or (min max)
  (is (null (motif-list '(x a b y a b z a b) nil)))
  (is (equal '(((a b) (1 2) (4 5) (7 8))) (motif-list '(x a b y a b z a b) nil :n 2)))
  (is (equal '(((a b) (1 2) (4 5) (7 8))) (motif-list '(x a b y a b z a b) nil :n '(2 3))))
  ;; :test, with and without a tolerance
  (let ((close (lambda (a b) (<= (abs (- a b)) 1))))
    (is (equal '(((60 62 64) (0 2) (4 6))) (motif-list '(60 62 64 70 61 62 65 72) nil :test close))))
  ;; 120 elements took about 30 s (a distance per window, even for exact repetitions)
  (let* ((*random-state* (seeded-random-state))
         (s (loop repeat 120 collect (nth (random 4) '(a b c d)))))
    (is (every #'(lambda (m) (>= (length (cdr m)) 2)) (motif-list s :length)))))

(test edit-dist-bounded
  ;; the bounded edit distance is the edit distance when it is not more than the bound, else nil
  (let ((*random-state* (seeded-random-state)))
    (is (loop repeat 500
              always (let* ((a (loop repeat (random 8) collect (nth (random 3) '(a b c))))
                            (b (loop repeat (random 8) collect (nth (random 3) '(a b c))))
                            (d (fv-morphologie::edit-dist1 a b))
                            (bound (random 6))
                            (r (fv-morphologie::edit-dist-bounded (coerce a 'simple-vector)
                                                                  (coerce b 'simple-vector)
                                                                  0 (length b) bound)))
                       (if (<= d bound) (eql r d) (null r)))))
    ;; from an offset in b
    (is (= 1 (fv-morphologie::edit-dist-bounded #(a b c) #(x x a b d x) 2 3 1)))
    (is (null (fv-morphologie::edit-dist-bounded #(a b c) #(x x a d d x) 2 3 1))))
  ;; a search with a tolerance, which took about 30 s for 120 elements
  (let* ((*random-state* (seeded-random-state))
         (s (loop repeat 120 collect (nth (random 4) '(a b c d)))))
    (is (every #'(lambda (m) (>= (length (cdr m)) 2)) (motif-list s :length :diss 0.25)))))

(test motif-find-strings-and-overlaps
  ;; strings had no applicable method
  (is (equal '((1 2) (4 5)) (motif-find "ab" "xabyAB")))
  (is (equal '((1 2)) (motif-find "ab" "xabyAB" :test #'char=)))
  (is (equal '((1 2) (4 5)) (motif-find "ab" "xacyab" :diss 0.5)))
  ;; occurrences may overlap (documented)
  (is (equal '((0 1) (1 2) (2 3)) (motif-find '(a a) '(a a a a))))
  ;; lists unchanged
  (is (equal '((0 1) (3 4) (6 7)) (motif-find '(a b) '(a b c a b d a b)))))

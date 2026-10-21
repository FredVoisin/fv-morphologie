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

(def-suite regressions :in fv-morphologie
  :description "Bugs fixed while moving to ASDF: they must not come back.")
(in-suite regressions)

(test no-self-recursive-wrappers
  ;; used to loop forever
  (signals error (num>alpha 2.5))
  (signals error (alpha>num #\a)))

(test dist-euclid-number-list
  ;; used to map over the number instead of the list
  (is (equal '(1 4 2) (dist-euclid 1 '(2 5 -1))))
  (is (equal '(1 4 2) (dist-euclid '(2 5 -1) 1))))

(test split-list-uses-marks
  (is (equal '((a b) (c)) (split '(a b x c) '(x)))))

(test motif-find-uses-edit-costs
  ;; used to call find-pos with too many arguments
  (finishes (motif-find '(a b) '(a b c) :change 2 :ins 1 :del 1)))

(test motif-list-default-output
  ;; (set out :length) used to fail at compile time
  (finishes (motif-list '(a b a b) nil)))

(test dist-multi-edit-keywords
  ;; positional arguments used to be passed as keywords
  (finishes (dist-multi-edit '((a 1) (b 2)) '((a 1) (c 2)) 1 :sub 2)))

(test graph>dot-dis-argument
  ;; used to refer to an undefined variable DISTORSION
  (finishes (with-output-to-string (s) (graph>dot '((a b 1)) s :dis 0.5))))

(test date-string-months
  ;; MONTHS used to be undefined
  (is (search "Jan" (date+time-string (encode-universal-time 0 0 12 15 1 2000)))))

(test graph-span-max-distance-edges
  ;; used to loop forever when the tree needed an edge of maximum length,
  ;; and printed its input
  (let* ((segs '((a b c) (a b d) (x y z) (x y w) (a b c d)))
         (tree (graph-span (dist-edit segs nil :norm t))))
    (is (= 4 (length tree)))
    (is (equal "" (with-output-to-string (*standard-output*)
                    (graph-span (dist-edit segs nil :norm t)))))))

(test more-than-two-classes
  ;; the number of classes used to be capped with (min 2 classes)
  (let ((*random-state* (seeded-random-state)))
    (is (= 3 (length (remove-duplicates
                      (class-num '((0 0) (0 1) (5 5) (5 6) (10 10) (10 11)) 3
                                 :centroids :iter 20))))))
  (is (= 3 (length (remove-duplicates
                    (class-sym '((a b c) (a b d) (x y z) (x y w) (a b c d)
                                 (k l m n o p) (k l m n o q))
                               3 :edit-norm))))))

(test class-num-nil-mode
  ;; (case mode (nil ...)) never matched: mode nil returned nil
  (let ((*random-state* (seeded-random-state)))
    (is (equal '(0 0 1 1)
               (first (class-num '((0 0) (0 1) (10 10) (10 11)) 2 nil))))))

(test class-num-1d-centroids-n-classes
  ;; used to return 2 classes whatever the number asked for
  (let ((data '(0 1 2 10 11 12 30 31 32 60 61 62)))
    (is (equal '(0 0 0 0 0 0 1 1 1 2 2 2)
               (first (class-num data 3 :1d-centroids))))
    (is (equal '(1.0 11.0 31.0 61.0)
               (second (class-num data 4 :1d-centroids))))
    (is (= 5 (length (remove-duplicates (first (class-num data 5 :1d-centroids))))))
    ;; no more classes than distinct values
    (is (equal '(0 0 1 1) (first (class-num '(1 1 2 2) 4 :1d-centroids))))))

(test alphanump-list
  ;; the list method returned a list of booleans, always true
  (is-true (fv-morphologie::alphanump '(#\a #\B #\1)))
  (is-false (fv-morphologie::alphanump '(#\! #\?)))
  (is-true (fv-morphologie::alphanump '(#\! #\a) '(#\!))))

(test class-sym-n-classes
  ;; equal longest edges were all cut at once: 3 classes when 2 were asked for
  (let ((segs '((a b c) (a b d) (x y z) (x y w) (a b c d) (k l m n o p) (k l m n o q))))
    (loop for n from 2 to 7
          do (is (= n (length (remove-duplicates (class-sym segs n :edit-norm))))))
    (is (equal '(0 0 1 1 0 2 2) (class-sym segs 3 :edit-norm)))
    ;; no more classes than distinct segments
    (is (= 7 (length (remove-duplicates (class-sym segs 8 :edit-norm)))))
    (is (equal '(0 1 0 1) (class-sym '((a b) (x y) (a b) (x y z)) 2 :edit-norm)))))

(test graph-part-n-parts
  (let ((g (list '(0 1 1) '(1 2 2) '(2 3 2) '(3 4 1))))
    ;; without n, all the longest edges are cut
    (is (equal '((0 1) (2) (4 3)) (nth-value 1 (fv-morphologie::graph-part g))))
    ;; the graph given used to be sorted destructively
    (is (equal '((0 1 1) (1 2 2) (2 3 2) (3 4 1)) g))
    (is (equal '(((0 1 1)) ((2 3 2) (3 4 1))) (fv-morphologie::graph-part g :distance 2)))
    (is (equal '((0 1) (2 3 4)) (nth-value 1 (fv-morphologie::graph-part g :distance 2))))
    (loop for n from 1 to 5
          do (is (= n (length (fv-morphologie::graph-part g :distance n)))))
    ;; no more parts than nodes
    (is (= 5 (length (fv-morphologie::graph-part g :distance 9))))
    ;; nodes may be any symbols
    (is (equal '((a b) (c d)) (nth-value 1 (fv-morphologie::graph-part '((a b 1) (b c 5) (c d 1)) :distance 2))))))

(test normalize-class
  ;; repeated values that are equal by test but not eql used to give nil
  (is (equal '(0 1 0) (fv-morphologie::normalize-class '((a) (b) (a)))))
  (is (equal '(0 1 0) (fv-morphologie::normalize-class '("x" "y" "x"))))
  (is (equal '(0 1 0) (fv-morphologie::normalize-class '(1 2 1.0) #'=)))
  ;; a test given as a symbol used to be evaluated as a variable
  (is (equal '(0 1 0) (fv-morphologie::normalize-class '(a b a) 'eq)))
  (is (equal '(0 1 2 0) (fv-morphologie::normalize-class "abca")))
  (is (equal '(0 1 1 2 0) (fv-morphologie::normalize-class '(3 1 1 2 3) #'=)))
  (is (null (fv-morphologie::normalize-class '()))))

(test nuees-finds-all-classes
  ;; a random start partition with equal barycentres used to give a single
  ;; class (17% of the runs for these 4 points)
  (loop for seed from 0 below 200
        do (let ((*random-state* (seeded-random-state seed)))
             (is (equal '(0 0 1 1)
                        (first (class-num '((0 0) (0 1) (10 10) (10 11)) 2 :centroids))))
             (is (= 3 (length (remove-duplicates
                               (first (class-num '((0 0) (0 1) (5 5) (5 6) (10 10) (10 11))
                                                 3 :centroids))))))))
  ;; no more classes than distinct points
  (is (equal '(0 0 1) (first (class-num '((0 0) (0 0) (1 1)) 3 :centroids)))))

(test filt-lowpass-double
  ;; (1- alpha) is alpha - 1, not 1 - alpha: constants were not kept,
  ;; and the last value was missing
  (is (every #'approx= '(5 5 5 5 5) (filt-lowpass '(5 5 5 5 5) 0.5 0.5)))
  (is (every #'approx= '(0 1 2 3 4 5) (filt-lowpass '(0 1 2 3 4 5) 0.3 0.7)))
  (is (every #'approx= '(0 10 10 12.5 8.125) (filt-lowpass '(0 10 0 10 0) 0.5 0.5)))
  (is (= 5 (length (filt-lowpass '(0 10 0 10 0) 0.5 0.5))))
  (is (equal '(3) (filt-lowpass '(3) 0.5 0.5)))
  ;; simple smoothing
  (is (every #'approx= '(0 5 2.5 6.25 3.125) (filt-lowpass '(0 10 0 10 0) 0.5)))
  (is (null (filt-lowpass '() 0.5))))

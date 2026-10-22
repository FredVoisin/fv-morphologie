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

(def-suite classification :in fv-morphologie
  :description "2. Classification of numbers and symbols.")
(in-suite classification)

(test class-num
  ;; two obvious clusters: the partition must separate them
  ;; (centroids start at random: use a fixed seed to keep the test deterministic)
  (let* ((*random-state* (seeded-random-state))
         (classes (first (class-num '((0 0) (0 1) (10 10) (10 11)) 2 :centroids))))
    (is (= (first classes) (second classes)))
    (is (= (third classes) (fourth classes)))
    (is (/= (first classes) (third classes)))))

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

(test class-sym-n-classes
  ;; equal longest edges were all cut at once: 3 classes when 2 were asked for
  (let ((segs '((a b c) (a b d) (x y z) (x y w) (a b c d) (k l m n o p) (k l m n o q))))
    (loop for n from 2 to 7
          do (is (= n (length (remove-duplicates (class-sym segs n :edit-norm))))))
    (is (equal '(0 0 1 1 0 2 2) (class-sym segs 3 :edit-norm)))
    ;; no more classes than distinct segments
    (is (= 7 (length (remove-duplicates (class-sym segs 8 :edit-norm)))))
    (is (equal '(0 1 0 1) (class-sym '((a b) (x y) (a b) (x y z)) 2 :edit-norm)))))

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

(test class-num-iter-frequencies
  ;; k + 1 runs were divided by k: the frequencies summed to (k + 1)/k
  (let ((*random-state* (seeded-random-state))
        (d '((0 0) (0 1) (5 5) (5 6) (10 10) (10 11))))
    (dolist (k '(1 2 5 20))
      (let ((r (class-num d 3 :centroids :iter (list k))))
        (is (approx= 1 (reduce #'+ (mapcar #'second r)) 1e-5))
        (is (every #'(lambda (x) (<= 0 (second x) 1)) r))
        ;; the most frequent first
        (is (apply #'>= (mapcar #'second r)))))
    ;; an integer iter gave a frequency of 2.0 for 1 run
    (is (approx= 1 (nth-value 1 (class-num d 3 :centroids :iter 1))))
    (is (<= (nth-value 1 (class-num d 3 :centroids :iter 20)) 1))
    ;; (0) divided by zero: at least one run
    (is (approx= 1 (second (first (class-num d 3 :centroids :iter (list 0))))))))

(test random-dot-in-bounds
  (let ((*random-state* (seeded-random-state)))
    ;; one coordinate too many: it always failed
    (is (= 2 (length (fv-morphologie::random-dot '((0 0) (1 0) (0 -1) (2 -0.1) (0.1 -2))))))
    ;; within the bounds, also for negative coordinates (it drew below the minimum)
    (is (loop repeat 500
              always (destructuring-bind (x y) (fv-morphologie::random-dot '((-5 -1) (-2 -3) (-4 -2)))
                       (and (<= -5 x -2) (<= -3 y -1)))))
    ;; a coordinate equal in all the points (random 0 failed)
    (is (equal '(3.0 3.0) (fv-morphologie::random-dot '((3 3) (3 3)))))
    (is (null (fv-morphologie::random-dot '())))))

(test class-num-modes-and-points
  (let ((*random-state* (seeded-random-state))
        (d '((0 0) (0 1) (10 10) (10 11))))
    ;; an unknown mode returned nil
    (signals error (class-num d 2 :foo))
    ;; modes by name, from any package
    (is (equal (first (class-num d 2 :centroids))
               (let ((*package* (find-package :cl-user)))
                 (first (class-num d 2 (intern "CENTROIDS" :cl-user))))))
    ;; one dimension: the centres are numbers (they were (x 0.0))
    (is (equal '(0.5 10.5) (second (class-num '(0 1 10 11) 2 :centroids))))
    ;; points of different dimensions
    (signals error (class-num '((0 0) (1 1 1)) 2 :centroids))
    ;; at least 2 classes
    (is (= 2 (length (remove-duplicates (first (class-num d 1 :centroids))))))
    ;; with an integer iter, two values: the partition and its frequency
    (multiple-value-bind (partition frequency) (class-num '(0 1 10 11) 2 :centroids :iter 3)
      (is (equal '(0 0 1 1) partition))
      (is (approx= 1 frequency)))))

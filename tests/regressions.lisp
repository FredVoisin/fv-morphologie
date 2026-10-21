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

(test graph-path-nodes-not-lengths
  ;; edge lengths were taken for nodes: 3 and 1 looked adjacent through (1 2 3)
  (let ((g '((0 1 2) (1 2 3) (2 3 1)))
        (s '((a b 1) (b c 2) (b d 1))))
    (is (equal '(3 2 1) (graph-path 3 1 g)))
    (is (equal '(0 1 2) (graph-path 0 2 g)))
    ;; adjacent nodes used to give the edge instead of the nodes
    (is (equal '(0 1) (graph-path 0 1 g)))
    (is (equal '(a b) (graph-path 'a 'b s)))
    ;; used to loop forever
    (is (equal '(c b a) (graph-path 'c 'a s)))
    (is (equal '(c b d) (graph-path 'c 'd s)))
    (is (equal '(a) (graph-path 'a 'a s)))
    (is (null (graph-path 'a 'z s)))
    (is (equal '("x" "y" "z") (graph-path "x" "z" '(("x" "y" 1) ("y" "z" 2)))))
    ;; paths to the leaves
    (is (equal '((a b c) (a b d)) (graph-path 'a nil s)))
    (is (equal '((1 0) (1 2 3)) (graph-path 1 nil g)))
    (is (equal '((a b c) (a b d) (c b d)) (graph-path nil nil s)))
    ;; dist-graph is 0 between adjacent nodes no more
    (is (= 1 (dist-graph 'a 'b s)))
    (is (= 4 (dist-graph 3 1 g)))))

(test filt-median-centred
  ;; the window looked ahead over w+1 values: steps and ramps were shifted
  (is (equal '(0 0 0 0 10 10 10 10) (filt-median '(0 0 0 0 10 10 10 10) 3)))
  (is (equal '(1 2 3 4 5 6 7 8) (filt-median '(1 2 3 4 5 6 7 8) 3)))
  ;; a peak is removed if narrower than half the window, kept otherwise
  (is (equal '(0 0 9 9 0 0 0) (filt-median '(0 0 9 9 0 0 0) 3)))
  (is (equal '(0 0 0 0 0 0 0 0 0) (filt-median '(0 0 0 9 9 0 0 0 0) 5)))
  ;; the first and last values are repeated at the edges: edge peaks are kept
  (is (equal '(9 0 0 0) (filt-median '(9 0 0 0) 3)))
  (is (equal '(0 0 0 9) (filt-median '(0 0 0 9) 3)))
  ;; odd windows only, so that the result is made of values of seq:
  ;; an even window is enlarged to the next odd size
  (is (equal (filt-median '(1 5 2 8 3) 5) (filt-median '(1 5 2 8 3) 4)))
  (is (equal '(1 2 3 3 3) (filt-median '(1 5 2 8 3) 4)))
  (is (subsetp (filt-median '(1 5 2 8 3 7 4) 4) '(1 5 2 8 3 7 4)))
  ;; same length, short lists filtered too, real window
  (is (equal '(5 5 5 5 5) (filt-median '(5 1 5 5 5) 3)))
  (is (equal '(2 2 3) (filt-median '(2 1 3) 3)))
  (is (equal '(1 1 1 1 1 1 1) (filt-median '(1 9 1 1 1 9 1) 3.0)))
  (is (equal '(4 2 7) (filt-median '(4 2 7) 1)))
  (is (null (filt-median '() 3))))

(test filt-mean-centred
  ;; the window looked ahead: steps and peaks were moved earlier
  (is (every #'approx= '(0 0 0 10/3 20/3 10 10 10) (filt-mean '(0 0 0 0 10 10 10 10) 3)))
  (is (every #'approx= '(0 0 3 3 3 0 0) (filt-mean '(0 0 0 9 0 0 0) 3)))
  ;; ramps are kept, ends included: the window shrinks symmetrically at the edges
  (is (every #'approx= '(1 2 3 4 5 6 7 8) (filt-mean '(1 2 3 4 5 6 7 8) 3)))
  (is (every #'approx= '(9 3 0 0 0) (filt-mean '(9 0 0 0 0) 3)))
  ;; an even window is enlarged to the next odd size
  (is (equal (filt-mean '(1 5 2 8 3 7) 5) (filt-mean '(1 5 2 8 3 7) 4)))
  ;; block mode: step = wind averages each block
  (is (every #'approx= '(2 2 2 5 5 5 8 8 8) (filt-mean '(1 2 3 4 5 6 7 8 9) 3 3)))
  (is (every #'approx= '(3 3 3 3 3 8 8 8 8 8) (filt-mean '(1 2 3 4 5 6 7 8 9 10) 3 5)))
  ;; a float window too small for the list used to divide by zero
  (is (every #'approx= '(1 2 3 4 5 6) (filt-mean '(1 2 3 4 5 6) 0.01)))
  (is (null (filt-mean '() 3))))

(test window-apply-list-order
  ;; list results were reversed within each window
  (is (equal '(1 2 3 4 5 6) (filt-fct '(1 2 3 4 5 6) #'identity 2 2))))

(test filt-fct-centred
  (flet ((mx (w) (apply #'max w)))
    ;; the window looked ahead, and the default step was 1 + wind/2
    (is (equal '(0 0 9 9 9 0 0 0) (filt-fct '(0 0 0 9 0 0 0 0) #'mx 3)))
    (is (equal (filt-fct '(0 0 0 9 0 0 0 0) #'mx 3) (filt-fct '(0 0 0 9 0 0 0 0) #'mx 3 1)))
    ;; wind 0 and 1 were forced to 2
    (is (equal '(1 2 3 4) (filt-fct '(1 2 3 4) #'mx 1)))
    (is (equal '(1 2 3 4) (filt-fct '(1 2 3 4) #'mx 0)))
    ;; a float wind above 0.5 failed, a ratio was ignored, a large step failed
    (is (equal '(1 3 5 7 8 8 8 8) (filt-fct '(1 2 3 4 5 6 7 8) #'mx 0.75)))
    (is (equal '(1 3 4 5 6 7 8 8) (filt-fct '(1 2 3 4 5 6 7 8) #'mx 1/4)))
    (is (equal '(4 4 4 4 4 8 8 8) (filt-fct '(1 2 3 4 5 6 7 8) #'mx 2 5))))
  ;; overlapping windows returning lists used to lengthen the result
  (is (equal '(1 2 3 4 5 6 7 8) (filt-fct '(1 2 3 4 5 6 7 8) #'identity 3)))
  (is (equal '(1 2 3 4 5 6 7 8) (filt-fct '(1 2 3 4 5 6 7 8) #'reverse 3 1)))
  (signals error (filt-fct '(1 2 3 4 5 6 7 8) (lambda (w) (remove 4 w)) 3 1))
  ;; blocks (step = wind) are replaced by the lists, whatever their length
  (is (equal '(3 2 1 6 5 4 8 7) (filt-fct '(1 2 3 4 5 6 7 8) #'reverse 3 3)))
  (is (equal '(1 2 3 5 6 7 8) (filt-fct '(1 2 3 4 5 6 7 8) (lambda (w) (remove 4 w)) 4 4)))
  ;; nil is a value, it used to remove the values
  (is (equal '(nil t nil t) (filt-fct '(1 2 3 4) (lambda (w) (evenp (car w))) 1))))

(test dist-multi-edit-weights
  (let ((a '((a 1 x) (b 2 y)))
        (b '((a 1 x) (c 2 z)))
        (*standard-output* (make-broadcast-stream)))
    ;; fewer events than dimensions, or fewer weights than dimensions, used to crash
    (is (approx= 1/3 (dist-multi-edit a b 1)))
    (is (approx= 1/3 (dist-multi-edit a b '(1 1))))
    (is (approx= 1/3 (dist-multi-edit a b '(1))))
    ;; more weights than dimensions used to scale the distance down
    (is (approx= 1/3 (dist-multi-edit a b '(1 1 1 1))))
    (is (approx= 1/6 (dist-multi-edit '((a 1) (b 2) (c 3)) '((a 1) (c 2) (c 3)) 1)))
    ;; a list of weights was not accepted
    (is (approx= 0.375 (dist-multi-edit a b '(2 1 1))))
    (is (approx= 0.375 (dist-multi-edit a b '(2 1))))
    (is (approx= 0.5 (dist-multi-edit a b '(1 0 0))))
    ;; all weights 0: all dimensions ignored (used to divide by zero)
    (is (approx= 0 (dist-multi-edit a b '(0 0 0))))
    (finishes (dist-multi-edit a b '(1 1) :uncom 0.5))
    ;; pairwise distances
    (is (approx= 1/3 (third (first (fv-morphologie::multi-edit-dist (list a b) nil 1)))))))

(test filt-local-rep-fixes
  ;; :linear used to modify the list given (and quoted constants)
  (let ((x (list 0 0 1 2 2 2 3 3 0 0)))
    (is (every #'approx= '(0 0.5 1 2 7/3 8/3 3 1.5 0 0) (filt-local-rep x :linear)))
    (is (equal '(0 0 1 2 2 2 3 3 0 0) x)))
  ;; with a tolerance, the ramp used to start from each value instead of the first of the run
  (let ((close (lambda (a b) (< (abs (- a b)) 0.2))))
    (is (every #'approx= '(1 5/3 7/3 3 4 5) (filt-local-rep '(1.0 1.1 1.05 3 3.1 5) :linear close)))
    (is (equal '(1.0 3 5) (filt-local-rep '(1.0 1.1 1.05 3 3.1 5) :delete close))))
  ;; repeated nils were not removed
  (is (equal '(a b nil c) (filt-local-rep '(a a b b nil nil c))))
  (is (equal '(nil a) (filt-local-rep '(nil nil a))))
  ;; :interp (PWGL) is :linear; an unknown mode was silently :delete
  (is (equal (filt-local-rep '(0 0 1 2 2 3) :linear) (filt-local-rep '(0 0 1 2 2 3) :interp)))
  (signals error (filt-local-rep '(1 1 2) :foo))
  ;; non-numeric data with :linear: repetitions removed
  (is (equal '(a b) (filt-local-rep '(a a b) :linear)))
  (is (equal '(5 5 5) (filt-local-rep '(5 5 5) :linear)))
  (is (null (filt-local-rep '() :linear))))

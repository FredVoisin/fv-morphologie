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

(test filt-noise-modes
  (let ((*random-state* (seeded-random-state)))
    ;; :nozero: all zeros used to crash; integer val could leave zeros
    (is (notany #'zerop (filt-noise '(0 0 0))))
    (is (every #'(lambda (x) (< 0 x 0.1)) (filt-noise '(0 0 0))))
    (is (loop repeat 200 never (find-if #'zerop (filt-noise '(0 10) :nozero 1))))
    (is (equal '(5 10) (cdr (filt-noise '(0 5 10)))))
    (signals error (filt-noise '(0 10) :nozero 0))
    ;; :relative was not implemented: noise within val * range
    (is (loop repeat 200
              always (every #'(lambda (x y) (<= (abs (- y x)) 20.0001))
                            '(100 200 0 50) (filt-noise '(100 200 0 50) :relative 0.1))))
    (is (every #'approx= '(5 5 5) (filt-noise '(5 5 5) :relative 0.1)))
    ;; :swap keeps the elements
    (is (equal '(1 2 3 4 5 6 7 8) (sort (filt-noise '(1 2 3 4 5 6 7 8) :swap 0.5) #'<)))
    ;; unknown modes used to return the list silently; empty lists crashed
    (signals error (filt-noise '(1 2 3) :foo))
    (is (null (filt-noise '())))
    (let ((l (list 0 1 2)))
      (filt-noise l :relative 0.5)
      (is (equal '(0 1 2) l)))))

(test filt-noise-function-mode
  ;; mode may be a function (x l) applied to each element x, l being the whole list
  (is (equal '(10 20 30) (filt-noise '(1 2 3) (lambda (x l) (declare (ignore l)) (* x 10)))))
  (is (equal '(3 3 3) (filt-noise '(a b c) (lambda (x l) (declare (ignore x)) (length l)))))
  (is (equal '(1/6 2/6 3/6) (filt-noise '(1 2 3) (lambda (x l) (/ x (reduce #'+ l))))))
  ;; replace each element by a random element of the list
  (let ((*random-state* (seeded-random-state)))
    (is (loop repeat 50
              always (subsetp (filt-noise '(a b c d)
                                          (lambda (x l) (declare (ignore x)) (nth (random (length l)) l)))
                              '(a b c d)))))
  (is (null (filt-noise '() (lambda (x l) (declare (ignore l)) x))))
  ;; the function gets a copy of the list: seq is not modified
  (let ((s (list 3 1 2)))
    (filt-noise s (lambda (x l) (setf (first l) 0) x))
    (is (equal '(3 1 2) s)))
  (signals error (filt-noise '(1 2) 'reverse)))

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
        (relative '((2 1 3) (5 -1 1) (9 1 7.5) (11 -1 2) (12 1 5))))
    ;; thres 0: the first-order landmarks, i.e. the local extrema
    (is (equal (int-signature r :minmax 0) (int-signature r :landmarks 0)))
    ;; absolute difference (default): the pairs (2 2.5) and (7 7.5) are removed
    (is (equal absolute (int-signature r :landmarks 0.6)))
    (is (equal absolute (int-signature r :landmarks 0.6 '(nil :absolute))))
    ;; the relative difference of Perng et al.: the pairs (2 2.5) and (8 7) are removed
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

(test signature-flat-extrema
  ;; a flat extremum is given once, at its first position, by all the methods
  ;; (:minmax ignored plateaus, :minflexmax marked each of their points as an inflexion)
  (dolist (m '(:minmax :minflexmax :major-extrema :landmarks))
    (is (equal '((1 -1 0)) (int-signature '(9 0 0 0 9) m 0)))
    (is (equal '((1 1 5)) (int-signature '(0 5 5 5 5 0) m 0)))
    ;; extrema alternate (:minmax gave two minima in a row)
    (is (equal '((1 1 4) (2 -1 0) (3 1 4) (5 -1 0)) (int-signature '(0 4 0 4 4 0 4) m 0))))
  ;; :minflexmax: a plateau which is not an extremum is one inflexion, at its first position
  (is (equal '((3 0 5)) (int-signature '(0 0 0 5 5 5) :minflexmax 0)))
  (is (equal '((1 0 3) (3 1 6)) (int-signature '(0 3 3 6 6 0) :minflexmax 0)))
  (is (null (int-signature '(0 0 0 5 5 5) :minmax 0)))
  ;; sequences without plateaus are unchanged
  (is (equal '((2 1 4) (4 -1 1) (6 1 6)) (int-signature '(0 2 4 3 1 2 6 5 0) :minmax 0))))

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

(test graph-name-from-path
  ;; a path without "/" crashed, and blanks cut the name
  (is (equal "out" (fv-morphologie::make-name-from-path "out.dot")))
  (is (equal "out" (fv-morphologie::make-name-from-path "out")))
  (is (equal "my graph" (fv-morphologie::make-name-from-path "my graph.dot")))
  (is (equal "c" (fv-morphologie::make-name-from-path "a.b/c.dot")))
  (is (equal "y" (fv-morphologie::make-name-from-path #p"/x/y.png")))
  (is (equal "span-graph" (fv-morphologie::make-name-from-path t))))

(test split-string-always-a-list
  ;; a string without marks used to be returned as is, not as a list
  (is (equal '("hello") (split-string "hello" ",")))
  (is (null (split-string "" ",")))
  (is (null (split-string "  " ",")))
  (is (equal '("a,b" "c") (split-string "a,b c" nil)))
  (is (equal '("12" "345" "6789") (split-string "12   345-6789" "- ")))
  ;; read-text: each line is a list, even of one word (as documented)
  (uiop:with-temporary-file (:pathname file :type "txt")
    (with-open-file (out file :direction :output :if-exists :supersede)
      (format out "1~%2 3~%~%4~%"))
    (is (equal '((1) (2 3) (4)) (read-text (namestring file))))
    (is (equal '("1" "2" "3" "4") (read-text (namestring file) :mode nil))))
  (uiop:with-temporary-file (:pathname file :type "txt")
    (with-open-file (out file :direction :output :if-exists :supersede)
      (format out "single"))
    (is (equal '("single") (read-text (namestring file) :mode nil)))))

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

(test edit-dist-rewrite
  ;; edit-dist2: whether an element exists in the other sequence is decided by test (it was eql)
  (is (= 2 (fv-morphologie::edit-dist2 '(1.0 2) '(2 1) :inex 1)))
  (is (= 4 (fv-morphologie::edit-dist2 '(1.0 2) '(2 1) :inex 1 :test #'eql)))
  ;; usual values
  (is (= 1 (fv-morphologie::edit-dist1 '(a b c) '(a b d))))
  (is (= 3 (fv-morphologie::edit-dist1 '() '(a b c))))
  (is (approx= 1/3 (fv-morphologie::edit-dist1 '(a b c) '(a b d) :normalized t)))
  ;; two empty sequences, normalized, divided by zero
  (is (approx= 0 (fv-morphologie::edit-dist1 '() '() :normalized t)))
  (is (approx= 0 (fv-morphologie::edit-dist2 '() '() :inex 1 :normalized t))))

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

(test slurp-file-multibyte
  ;; file-length counts bytes: a UTF-8 file gave one NUL (SBCL) or space (ECL)
  ;; per extra byte at the end of the string
  (uiop:with-temporary-file (:pathname file :type "txt")
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "~a~%~a" (coerce (list (code-char 233) #\t (code-char 233)) 'string) "abc"))
    (let ((s (fv-morphologie::slurp-file file))
          (lines (fv-morphologie::read-text-lines file)))
      (is (equal 2 (length lines)))
      (is (= (length s) (+ (reduce #'+ (mapcar #'length lines)) (1- (length lines)))))
      (is (notany #'(lambda (c) (member (char-code c) '(0))) s))
      (is (char= #\c (char s (1- (length s)))))))
  ;; an empty file
  (uiop:with-temporary-file (:pathname file :type "txt")
    (is (equal "" (fv-morphologie::slurp-file file)))
    (is (null (fv-morphologie::read-text-lines file)))))

(test read-text-charcode
  (flet ((write-file (file text)
           (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
             (write-string text out))))
    (uiop:with-temporary-file (:pathname file :type "txt")
      (write-file file (format nil "ab-cd~cef~%gh!ij~%" #\Tab))
      ;; newlines and tabulations separate the segments (they stayed inside them)
      (is (equal '((97 98 45 99 100) (101 102) (103 104 33 105 106))
                 (read-text file :mode :charcode)))
      (is (equal '((97 98) (99 100) (101 102) (103 104) (105 106))
                 (read-text file :mode :charcode :sep "-!")))
      (is (equal (read-text file :mode :charcode :sep "-!")
                 (read-text file :mode :charcode :sep (list #\- #\!))))
      (is (equal '((97 98 45 99 100) (101 102) (103 104) (105 106))
                 (read-text file :mode :charcode :sep (list :ignore #\-))))
      ;; the old PWGL modes, and any unknown mode, are errors (:ascii-7b gave the lines)
      (signals error (read-text file :mode :ascii-7b))
      (signals error (read-text file :mode :foo)))
    (uiop:with-temporary-file (:pathname file :type "txt")
      ;; accented letters are letters (they were separators without 8 bits)
      (write-file file (format nil "~a,~%~a~%"
                               (coerce (list (code-char 233) #\t (code-char 233)) 'string)
                               (coerce (list (code-char 231) #\a) 'string)))
      (is (equal '((233 116 233) (231 97)) (read-text file :mode :charcode :sep ",")))
      (is (equal '((233 116 233) (231 97)) (read-text file :mode :charcode :sep (list :ignore)))))))

(test graph>dot-output
  (flet ((dot (graph &rest keys)
           (with-output-to-string (s) (apply #'graph>dot graph s keys))))
    ;; len was formatted on 3 characters first: 12.5 gave 13., 0.123456 gave .12
    (let ((d (dot '((a b 12.5) (b c 0.123456) (c d 1/3)))))
      (is (search "len=12.500000" d))
      (is (search "len=0.123456" d))
      (is (search "len=0.333333" d))
      ;; labels are quoted (1/3 unquoted is not valid DOT)
      (is (search "label=\"1/3\"" d)))
    ;; the case of the shape is kept (Mrecord gave mrecord)
    (is (search "shape=\"Mrecord\"" (dot '((a b 1)) :shape "Mrecord")))
    ;; no package prefix for the names of the nodes
    (let ((*package* (find-package :cl-user)))
      (is (search "\"A\" -- \"B\"" (dot (list (list 'fv-morphologie/tests::a 'fv-morphologie/tests::b 1))))))
    ;; quotes in names are escaped
    (is (search "\"q\\\"t\"" (dot '(("z" "q\"t" 2)))))
    ;; only edges of length 0, or no edge, used to fail
    (is (search "\"A\" -- \"B\" [len=1.000000]" (dot '((a b 0) (b c 0)))))
    (is (search "graph \"span-graph\" {" (dot '())))))

(test graph>dot-neato
  ;; draws a graph with neato, if Graphviz is installed (else the test is skipped)
  (if (ignore-errors (uiop:run-program '("neato" "-V") :output nil :error-output nil) t)
      (uiop:with-temporary-file (:pathname tmp :type "svg" :keep nil)
        (let* ((svg (namestring tmp))
               (dot (namestring (make-pathname :type "dot" :defaults tmp))))
          ;; 1/3 as an unquoted label was a syntax error for Graphviz: no image was made
          (graph>dot '((a b 1) (a c 2.5) (b d 0) (c e 1/3)) svg :shape "Mrecord")
          (is (probe-file svg))
          (is (search "<svg" (uiop:read-file-string svg)))
          (ignore-errors (delete-file dot))))
      (skip "neato (Graphviz) is not installed")))

(test graph-span-kruskal
  (flet ((total (tree) (reduce #'+ (mapcar #'third tree))))
    ;; deterministic, edges by increasing distance (Prim started from a random vertex)
    (is (equal '((0 1 1) (2 3 1) (1 2 2)) (graph-span '((0 1 1) (0 2 4) (1 2 2) (2 3 1)))))
    (let ((square '((a b 1) (b c 1) (c d 1) (d a 1) (a c 2) (b d 2))))
      (is (equal '((a b 1) (b c 1) (c d 1)) (graph-span square)))
      (is (loop repeat 20 always (equal (graph-span square) (graph-span square)))))
    ;; distances 0, or no edge, used to fail
    (is (equal '((a b 0) (b c 0)) (graph-span '((a b 0) (b c 0)))))
    (is (null (graph-span '())))
    ;; vertices compared with equalp (strings gave an incomplete tree)
    (is (equal '(("x" "y" 1) ("y" "z" 2)) (graph-span '(("x" "y" 1) ("y" "z" 2) ("x" "z" 3)))))
    ;; a graph which is not connected: a spanning forest (an edge was lost)
    (let ((*standard-output* (make-broadcast-stream)))
      (is (equal '((a b 1) (c d 2)) (graph-span '((a b 1) (c d 2))))))
    ;; minimal: the total length of the tree of 6 points on a line
    (is (= 5 (total (graph-span (dist-euclid '((0) (1) (2) (3) (4) (5)) nil)))))
    ;; 320 points (51040 distances) took minutes
    (let* ((*random-state* (seeded-random-state))
           (pts (loop repeat 320 collect (list (random 100) (random 100)))))
      (is (= 319 (length (graph-span (dist-euclid pts nil))))))))

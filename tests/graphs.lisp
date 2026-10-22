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

(def-suite graphs :in fv-morphologie
  :description "Graphs: spanning trees, paths, measures, partitions.")
(in-suite graphs)

(defparameter *chain* '((a b 1) (b c 2) (c d 1)))

(test graph-span
  ;; minimal spanning tree: edge (0 2 4) is dropped (edge order may vary)
  (let ((*standard-output* (make-broadcast-stream)))
    (is (null (set-exclusive-or '((0 1 1) (1 2 2) (2 3 1))
                                (graph-span (copy-tree '((0 1 1) (0 2 4) (1 2 2) (2 3 1))))
                                :test #'equal)))))

(test graph-path
  (is (equal '(a b c d) (graph-path 'a 'd *chain*))))

(test graph-length
  (is (= 4 (graph-length *chain*))))

(test graph-degree
  (is (= 2 (graph-degree 'b *chain*))))

(test graph-nodes
  (is (equal '(a b c d) (graph-nodes *chain*)))
  ;; nodes joined by a zero-length edge are nodes too
  (is (equal '(a b c) (graph-nodes '((a b 1) (b c 0)))))
  (is (equal '(0 1 2 3) (graph-nodes '((2 3 1) (0 1 1) (1 2 2)))))
  ;; mixed nodes: numbers first, then the others by name
  (is (equal '(1 2.5 10 a "b" c) (graph-nodes '((c 10 1) ("b" 2.5 1) (a 1 2))))))

(test graph-extrem
  (is (null (set-exclusive-or '(a d) (graph-extrem *chain*)))))

(test graph-span-max-distance-edges
  ;; used to loop forever when the tree needed an edge of maximum length,
  ;; and printed its input
  (let* ((segs '((a b c) (a b d) (x y z) (x y w) (a b c d)))
         (tree (graph-span (dist-edit segs nil :norm t))))
    (is (= 4 (length tree)))
    (is (equal "" (with-output-to-string (*standard-output*)
                    (graph-span (dist-edit segs nil :norm t)))))))

(test graph-part-n-parts
  (let ((g (list '(0 1 1) '(1 2 2) '(2 3 2) '(3 4 1))))
    ;; without n, all the longest edges are cut
    (is (equal '((0 1) (2) (3 4)) (nth-value 1 (fv-morphologie::graph-part g))))
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

(test graph-part-longest-edges
  ;; without n: all the edges of maximum length are cut
  (is (equal '(((a b 1)) ((c d 1))) (graph-part '((a b 1) (b c 5) (c d 1)))))
  ;; strings, and graphs which are not connected, looped forever
  (is (equal '(("x" "y") ("z" "w")) (nth-value 1 (graph-part '(("x" "y" 1) ("y" "z" 5) ("z" "w" 1))))))
  (is (equal '((a b) (c) (d)) (nth-value 1 (graph-part '((a b 1) (c d 2))))))
  ;; nodes were repeated in a part ((e d e))
  (is (equal '((a) (b c) (d e) (f)) (nth-value 1 (graph-part '((a b 3) (b c 1) (c d 3) (d e 1) (e f 3))))))
  ;; a single edge gave only the first value
  (is (equal '((a) (b)) (nth-value 1 (graph-part '((a b 1))))))
  (is (null (graph-part '())))
  ;; an unknown mode printed a message and returned nil
  (signals error (graph-part '((a b 1) (b c 2)) :foo)))

(test graph-nodes-equal-and-shortest-paths
  ;; strings differing only in case are different nodes (equalp took them for one)
  (is (null (graph-path "A" "c" '(("A" "b" 1) ("a" "c" 1)))))
  (is (equal '("A" "b") (graph-path "A" "b" '(("A" "b" 1) ("a" "c" 1)))))
  (is (equal '("A" "a" "b" "c") (graph-nodes '(("A" "b" 1) ("a" "c" 1)))))
  (is (= 2 (length (graph-span '(("A" "b" 1) ("a" "b" 1) ("A" "a" 5))))))
  (is (= 1 (graph-degree "a" '(("A" "b" 1) ("a" "c" 1)))))
  ;; a graph with a cycle: the shortest path by distance (it was the one with fewest edges)
  (let ((g '((a b 1) (b c 1) (c d 1) (a d 10))))
    (is (equal '(a b c d) (graph-path 'a 'd g)))
    (is (= 3 (dist-graph 'a 'd g))))
  ;; no path: nil (dist-graph gave 0)
  (is (null (dist-graph 'a 'c '((a b 1) (c d 1)))))
  (is (= 0 (dist-graph 'a 'a '((a b 1)))))
  ;; a path of 3000 nodes took 0.2 s, all the leaf-to-leaf paths of 300 nodes 2 s
  (let ((g (loop for i below 2999 collect (list i (1+ i) 1))))
    (is (= 3000 (length (graph-path 0 2999 g))))
    (is (= 2999 (dist-graph 0 2999 g)))))

(test graph-degree-forms
  (let ((g '((a b 1) (b c 2) (b d 1))))
    (is (= 3 (graph-degree 'b g)))
    (is (= 0 (graph-degree 'z g)))
    ;; a list of nodes, and all the nodes in the order of graph-nodes
    (is (equal '(3 1) (graph-degree '(b d) g)))
    (is (equal (mapcar #'(lambda (n) (graph-degree n g)) (graph-nodes g)) (graph-degree nil g)))
    (is (equal '(1 3 1 1) (graph-degree nil g)))))

(test graph>matrix-vertices
  ;; vertices were looked up with eq: string vertices (and bignums) failed
  (is (equal '((0 1 0) (1 0 2) (0 2 0))
             (fv-morphologie:graph>matrix (list (list (copy-seq "x") (copy-seq "y") 1)
                                                (list (copy-seq "y") (copy-seq "z") 2)))))
  (is (equal '((0 5) (5 0)) (fv-morphologie:graph>matrix (list (list (expt 10 30) 1 5)))))
  ;; rows and columns in the order of graph-nodes
  (is (equal '((0 1 4) (1 0 2) (4 2 0)) (fv-morphologie:graph>matrix '((0 1 1) (1 2 2) (0 2 4)))))
  (is (equal '((0 1 0) (1 0 2) (0 2 0)) (fv-morphologie:graph>matrix '((a b 1) (b c 2)))))
  (is (null (fv-morphologie:graph>matrix '()))))

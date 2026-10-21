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
  :description "Graphs and dot output.")
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

(test graph>dot-stream
  (let ((dot (with-output-to-string (s) (graph>dot '((a b 1) (b c 0)) s))))
    (is (search "graph \"span-graph\" {" dot))
    (is (search "\"A\" -- \"B\"" dot))
    (is (search "\"B\" -- \"C\"" dot))))

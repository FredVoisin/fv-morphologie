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

;;;; graphs.lisp
;;;;
;;;; Trees and graphs: minimum spanning tree, paths, lengths, degrees,
;;;; adjacency matrices, partitions.

(in-package :fv-morphologie)

(defgeneric graph2matrix (graph)
  (:documentation "represents a graph (list of (v1 v2 edge)) as a standart adjacent matrix"))

(defmethod graph2matrix ((graph list))
  (adj-matrix graph))

;;; TREES AND GRAPHS
; 1. minimum spanning tree (in french: arbre de longueur minimum ou arbre de recouvrement)
; using Prim's algorithm

(defun remove-ifnot-1 (set)
  (remove-if #'(lambda (s) (not (= 1 (length s)))) set))

(defun remove-ifonly-1 (set)
  (remove-if #'(lambda (s) (= 1 (length s))) set))

(defun extremites (points tree)
  (mapcar #'car
	  (remove-ifnot-1
	   (mapcar #'(lambda (a) (remove 'nil a))
		   (mapcar #'(lambda (point)
                               (mapcar #'(lambda (x)
                                           (when (not (equalp 'nil (member point x))) x))
                                       tree))
			   points)))))

(defun tronc (extrem noeuds)
  (dolist (e extrem (remove-duplicates (flat-1 noeuds) :test 'equalp))
    (dotimes (n (length noeuds))
      (setf (nth n noeuds) (remove-if #'(lambda (x) (equalp e x)) (nth n noeuds))))))

(defun commun (list)
  "Presuppose qu'il y a au moins un elt commun a toutes les sous-listes de list."
  (car (remove-if #'numberp (intersection (car list) (cadr list)))))

(defun noeuds (points tree)
  (remove-ifonly-1
   (mapcar #'(lambda (a) (remove 'nil a))
	   (mapcar #'(lambda (point) (mapcar #'(lambda (x) (when (not (equalp 'nil (member point x))) x)) tree))
		   points))))

(defun spanning-tree (graph &optional verbose)
  "Minimum spanning tree of graph, a list of edges (vertex-a vertex-b distance), by Kruskal's algorithm:
the edges are taken by increasing distance (in their order in graph for equal distances), and kept if
they join two parts not yet joined. Vertices are compared with equal. The result is deterministic;
if graph is not connected, it is a minimum spanning forest (one tree for each connected part)."
  (let ((parent (make-hash-table :test #'equal))
        (tree nil)
        (vertices 0))
    (labels ((root (x)
               (let ((p (gethash x parent x)))
                 (if (equal p x) x (setf (gethash x parent) (root p))))))
      (dolist (e graph)
        (dolist (v (list (first e) (second e)))
          (unless (nth-value 1 (gethash v parent))
            (setf (gethash v parent) v)
            (incf vertices))))
      (dolist (e (stable-sort (copy-list graph) #'< :key #'third))
        (let ((ra (root (first e))) (rb (root (second e))))
          (unless (equal ra rb)
            (setf (gethash ra parent) rb)
            (when verbose (message e))
            (push e tree))))
      (when (and graph (< (length tree) (1- vertices)))
        (message (format nil "graph-span: the graph is not connected: minimum spanning forest of ~D trees."
                         (- vertices (length tree)))))
      (nreverse tree))))

(defgeneric minimum-spanning-tree (graph &optional verbose)
  (:documentation "Returns the span tree for a list of points according to the list of their distance.
The list of distances must have the form: ((xi yi di) ...), for instance a semi-matrix of distances.
The edges of the tree are given by increasing distance. See spanning-tree (Kruskal's algorithm;
the former version used Prim's algorithm, after E. Diday & all, 1982 : Elements d'analyse de donnees,
Dunod, Paris. pp. 110-111)."))

(defmethod minimum-spanning-tree ((graph list) &optional (verbose nil))
  (spanning-tree graph verbose))

(defun graph-neighbours (graph)
  "Hash table (test equal) of the neighbours of each node of graph, a list of edges
(node-a node-b distance): a list of (neighbour . distance), in the order of the edges."
  (let ((nb (make-hash-table :test #'equal)))
    (dolist (e graph nb)
      (push (cons (second e) (third e)) (gethash (first e) nb))
      (push (cons (first e) (third e)) (gethash (second e) nb)))
    (maphash #'(lambda (k v) (setf (gethash k nb) (nreverse v))) nb)
    nb))

(defun shortest-paths (neighbours start)
  "Dijkstra's algorithm from start: two hash tables (test equal), the distance of each node
reachable from start, and its predecessor on a shortest path. Distances must be positive or 0."
  (let ((dist (make-hash-table :test #'equal))
        (pred (make-hash-table :test #'equal))
        (done (make-hash-table :test #'equal))
        (frontier (list start)))
    (setf (gethash start dist) 0)
    (loop while frontier
          do (let* ((n (let ((best (first frontier)))
                         (dolist (x (rest frontier) best)
                           (when (< (gethash x dist) (gethash best dist)) (setf best x)))))
                    (dn (gethash n dist)))
               (setf frontier (delete n frontier :test #'equal :count 1)
                     (gethash n done) t)
               (dolist (m (gethash n neighbours))
                 (let ((node (car m)) (d (+ dn (cdr m))))
                   (unless (gethash node done)
                     (multiple-value-bind (old found) (gethash node dist)
                       (when (or (not found) (< d old))
                         (unless found (push node frontier))
                         (setf (gethash node dist) d
                               (gethash node pred) n))))))))
    (values dist pred)))

(defun path-to (pred start end)
  "The nodes of the path from start to end, from the predecessors pred (see shortest-paths)."
  (let ((path (list end)))
    (loop until (equal (first path) start)
          do (push (gethash (first path) pred) path))
    path))

(defun find-path (tree start end)
  "The nodes of the shortest path (by distance) from start to end in tree, a list of edges
(node-a node-b distance), or nil if there is no path. Nodes are compared with equal.
In a tree there is only one path; in other graphs, Dijkstra's algorithm gives the shortest."
  (if (equal start end)
      (list start)
      (multiple-value-bind (dist pred) (shortest-paths (graph-neighbours tree) start)
        (when (nth-value 1 (gethash end dist))
          (path-to pred start end)))))

(defun graph-distance (tree start end)
  "The length of the shortest path from start to end in tree, or nil if there is no path."
  (if (equal start end)
      0
      (values (gethash end (shortest-paths (graph-neighbours tree) start)))))

(defun tree-leaf-nodes (tree)
  "The nodes of degree 1 of tree, a list of edges (node-a node-b distance)."
  (remove-if-not #'(lambda (v)
                     (= 1 (count-if #'(lambda (e) (or (equal v (first e)) (equal v (second e))))
                                    tree)))
                 (all-vertices tree)))

(defun paths-from (start ends neighbours)
  "The shortest paths from start to each of ends reachable from it (one search from start)."
  (multiple-value-bind (dist pred) (shortest-paths neighbours start)
    (loop for e in ends
          when (nth-value 1 (gethash e dist))
            collect (path-to pred start e))))

(defun paths-to-leaves (start tree)
  "The paths from start to each leaf of tree."
  (paths-from start (remove start (tree-leaf-nodes tree) :test #'equal) (graph-neighbours tree)))

(defun all-paths (tree)
  "The paths between all the pairs of leaves of tree."
  (let ((nb (graph-neighbours tree)))
    (loop for (l . others) on (tree-leaf-nodes tree)
          append (paths-from l others nb))))

(defgeneric tree-path (from to tree)
  (:documentation "path from vertex to vertex in tree."))

(defmethod tree-path ((from number) (to number) (tree list))
  (find-path tree from to))

(defmethod tree-path ((from symbol) (to symbol) (tree list))
  (find-path tree from to))

(defmethod tree-path ((from null) (to null) (tree list))
  (all-paths tree))

(defmethod tree-path ((from t) (to null) (tree list))
  (paths-to-leaves from tree))

;; nil is a symbol: without this method, (tree-path 'a nil tree) would look for a node named nil
(defmethod tree-path ((from symbol) (to null) (tree list))
  (paths-to-leaves from tree))

(defmethod tree-path ((from null) (to t) (tree list))
  (paths-to-leaves to tree))

(defmethod tree-path ((from list) (to null) (tree list))
  (mapcar #'(lambda (v) (paths-to-leaves v tree)) from))

(defmethod tree-path ((from null) (to list) (tree list))
  (mapcar #'(lambda (v) (paths-to-leaves v tree)) to))

(defmethod tree-path ((from list) (to list) (tree list))
  (mapcar #'(lambda (s)
              (remove-duplicates (mapcar #'(lambda (e)
                                             (find-path tree s e)) to)
                                 :test #'equal))  from))

(defmethod tree-path ((from string) (to string) (tree list))
  (find-path tree from to))

;; a tree is a list of list of edges each defined by two vertices, such as: ( (a b) (b c) (b d) (c e)...) 
; any edge can be defined by some properties in the rest of the list of its vertices ;
; for instance: (a b .1 2 z) is an edge between vertices a and b with (.12 z) as properties of the edge.

(defun vertex< (a b)
  "Order for vertices of any type: numbers first (by value), then all others by name."
  (flet ((name (x) (if (or (stringp x) (symbolp x) (characterp x)) (string x) (princ-to-string x))))
    (cond ((and (realp a) (realp b)) (< a b))
          ((realp a) t)
          ((realp b) nil)
          (t (string-lessp (name a) (name b))))))

(defun all-vertices (tree)
  (let ((v (remove-duplicates (apply #'append (mapcar #'(lambda (x) (subseq x 0 2)) tree)) :test #'equal)))
    (sort v #'vertex<)))

(defun min-total-length (tree)
  (apply #'+ (mapcar #'caddr tree)))

(defgeneric tree-minlen (tree)
  (:documentation "Minimum total length of a tree (sum of all edge's length)."))

(defmethod tree-minlen ((tree list))
  (min-total-length tree))

(defun tree-degree (vertex tree)
  (count-if #'(lambda (x) (or (equal vertex (first x)) (equal vertex (second x)))) tree))

(defgeneric tree-deg (vertex tree)
  (:documentation "Degree of a vertex in a tree."))

(defmethod tree-deg ((vertex t) (tree list))
  (tree-degree vertex tree))

(defmethod tree-deg ((vertex list) (tree list))
  (mapcar #'(lambda (x) (tree-deg x tree)) vertex))

(defmethod tree-deg ((vertex null) (tree list))
  (tree-deg (all-vertices tree) tree))

(defun nodes (tree &optional (min-degree 2))
  (let* ((e  (mapcar #'butlast (remove-if #'(lambda (x) (zerop (caddr x))) tree)))  ;; remove if dist = 0
         (v (remove-duplicates (apply #'append e) :test #'equal))
         (r (list)))
    (dolist (k v)
      (push (list k (remove-if-not #'(lambda (x) (member k x :test #'equal)) e)) r))
    (mapcar #'car (remove-if #'(lambda (x) (< (length (cadr x)) min-degree)) r))))

(defgeneric tree-nodes (tree &optional min-deg)
   (:documentation "List of nodes in a tree."))

(defmethod tree-nodes ((tree list) &optional (min-deg 2))
  (nodes tree min-deg))

(defun leaves (tree)
  (let* ((e  (mapcar #'butlast tree))
         (v (remove-duplicates (apply #'append e) :test #'equal))
         (r (list)))
    (dolist (k v)
      (push (list k (remove-if-not #'(lambda (x) (member k x :test #'equal)) e)) r))
    (mapcar #'car (remove-if-not #'(lambda (x) (= (length (cadr x)) 1)) r))))

(defgeneric tree-leaves (tree)
   (:documentation "List of leaves in a tree."))

(defmethod tree-leaves ((tree list))
  (leaves tree))

(defgeneric tree-dist (vertex1 vertex2 tree)
   (:documentation "Distance between vertex1 and vertex2 in tree."))

(defmethod tree-dist ((vertex1 t) (vertex2 t) (tree list))
  ;; the length of the shortest path, nil if there is none (it gave 0)
  (graph-distance tree vertex1 vertex2))

(defmethod tree-dist ((vertex1 t) (vertex2 list) (tree list))
  (mapcar #'(lambda (x) (tree-dist vertex1 x tree)) vertex2))

(defmethod tree-dist ((vertex1 t) (vertex2 null) (tree list))
  (mapcar #'(lambda (x) (length (tree-path vertex1 x tree))) (remove vertex1 (all-vertices tree) :test #'equal)))

;;Strahler number to do


;; graphs - suite

(defun adj-matrix (l-graph)
  "The symmetric adjacency matrix of l-graph, a list of edges (v1 v2 distance), as a list of rows:
rows and columns in the order of all-vertices (graph-nodes), the distance of each edge, 0 elsewhere.
Vertices are compared with equal (eq failed on strings, which are not eq when equal)."
  (let* ((v (all-vertices l-graph))
         (m (make-list (length v))))
    (dotimes (i (length m))
      (setf (nth i m) (make-list (length v) :initial-element 0)))
    (dolist (g l-graph m)
      (let ((i (position (car g) v :test #'equal))
            (j (position (cadr g) v :test #'equal))
            (d (caddr g)))
        (setf (nth j (nth i m)) d
              (nth i (nth j m)) d)))))

(defun regroup-if-intersection (ldl)
  (loop for i from 0 to (- (length ldl) 2)
     do (when (nth i ldl)
          (loop for j from (1+ i) to (1- (length ldl))
	     do (when (nth j ldl)
                  (when (intersection (nth i ldl) (nth j ldl))
                    (setf (nth i ldl) (append (nth i ldl) (nth j ldl))
                          (nth j ldl) nil))))))
  (values (remove nil ldl)))

(defun part-tree-dist (tree)
  "Split a tree into partitions by cutting all its edges of maximum distance (see part-tree-n)."
  (if (null tree)
      (values nil nil)
      (let ((longest (reduce #'max (mapcar #'third tree))))
        (part-tree-n tree (1+ (count longest tree :key #'third :test #'=))))))

(defgeneric part-graph (graph mode &optional n)
  (:documentation "Does a partition of a graph <graph>, into <n> parts if <n> is given."))

(defmethod part-graph ((graph list) (mode symbol) &optional (n nil))
  (case mode
    ((nil :distance) (if n (part-tree-n graph n) (part-tree-dist graph)))
    (t (error "graph-part: unknown mode ~S, use :distance." mode))))

(defun tree-components (n-nodes edges)
  "Connected components of the graph of nodes 0..n-nodes-1 and edges (i j d):
for each node, the class of its component, numbered from 0 by order of first node."
  (let ((root (make-array n-nodes)))
    (dotimes (i n-nodes) (setf (aref root i) i))
    (labels ((find-root (i)
               (if (= i (aref root i)) i
                   (setf (aref root i) (find-root (aref root i))))))
      (dolist (e edges)
        (let ((a (find-root (first e))) (b (find-root (second e))))
          (unless (= a b) (setf (aref root (max a b)) (min a b)))))
      (let ((roots (loop for i below n-nodes collect (find-root i))))
        (normalize-class roots #'=)))))

(defun part-tree-n (tree n)
  "Split a tree into n partitions by cutting its n-1 longest edges (the first one in the tree if equal).
Returns the edges of each partition (((node)) for a single node) and the nodes of each partition."
  (let* ((nodes (remove-duplicates (append (mapcar #'first tree) (mapcar #'second tree))
                                   :test #'equal :from-end t))
         (cut (subseq (stable-sort (copy-list tree) '> :key #'third)
                      0 (min (max 0 (1- n)) (length tree))))
         (kept (remove-if #'(lambda (e) (member e cut :test #'eq)) tree))
         (comp (tree-components (length nodes)
                                (mapcar #'(lambda (e) (list (position (first e) nodes :test #'equal)
                                                            (position (second e) nodes :test #'equal)))
                                        kept)))
         (parts (loop for c from 0 to (if comp (apply #'max comp) -1)
                      collect (loop for x in nodes for k in comp when (= k c) collect x))))
    (values (mapcar #'(lambda (p)
                        (or (remove-if-not #'(lambda (e) (member (first e) p :test #'equal)) kept)
                            (list p)))
                    parts)
            parts)))

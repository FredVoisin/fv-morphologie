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

;;;; api/evaluation.lisp
;;;;
;;;; Public functions: evaluation (positions, motifs, distances, information)
;;;; and graph measures.

(in-package :fv-morphologie)

(defgeneric mark-position (seq mark &optional test cons)
  (:documentation "MARK-POSITION: returns all positions of marks in seq."))

(defmethod mark-position ((seq t) (mark t) &optional (test #'equalp) (cons t))
  (all-pos seq mark test cons))

(defgeneric mark-list (seq mark &key mark-t seg-t)
  (:documentation "MARK-LIST: returns the list of all segments and their position in <seq> beginning with some marks defined with argument <mark>.
If no argument mark (nil), considers all different symbols in <seq>.
Option :mark-t defines the test function for searching the marks (equalp by default).
Option :group-t defines the test function for comparing the different segments."))

(defmethod mark-list ((seq t) (mark t) &key (mark-t #'equalp) (seg-t #'equalp))
  (when (and (symbolp mark-t) (not (boundp mark-t)))
    (setf mark-t (eval `(function ,mark-t))))
  (when (and (symbolp seg-t) (not (boundp seg-t)))
    (setf seg-t (eval `(function ,seg-t))))
  (mark-pos seq mark mark-t seg-t))

(defgeneric motif-find (motif seq &key diss l-var change ins del uncom test)
  (:documentation "MOTIF-FIND: returns all positions of a <motif> into a sequence <seq>.
Argument :diss is the threshold (from 0 to 1.0) of dissimilarity according to the normalized editing distance under which two different segments are considered to be the same ;
argument :l-var is the threshold for variation in length of the motifs to be compared ;
arguments :change :ins/sup :uncom and :test for tuning the editing distance (see dist-edit)."))

(defmethod motif-find ((motif t) (seq t) &key (diss 0) (l-var 0) (change 1) (ins 1) (del 1) (uncom 0) (test #'equalp))
  (find-pos motif seq diss l-var
            #'(lambda (a b) (edit-dist a b change ins del uncom t test))))

(defmethod motif-find ((motif string) (seq string) &key (diss 0) (l-var 0) (change 1) (ins 1) (del 1) (uncom 0) (test #'equalp))
  ;; a string is searched as a list of characters (there was no method for strings)
  (motif-find (coerce motif 'list) (coerce seq 'list) :diss diss :l-var l-var
              :change change :ins ins :del del :uncom uncom :test test))

(defgeneric motif-list (seq out &key diss l-var n change insert delete uncom test)
  (:documentation "MOTIF-LIST: returns the list of all motifs found into <seq> where a motif is any segment repeted at least one time according to editing distance.
Output can be sorted according to the length of segment (:length, or nil) or to their frequency (:freq); another output is an error.
Argument :diss is a threshold (from 0 to 1.0) of dissimilarity according to the normalized editing distance under which two different segments are considered to be the same ;
argument :l-var is the threshold for variation in length of the segments to be compared ;
argument :n is the maximum length of the motifs (lengths 2 to n), or a list (min max) (default: 3 to half the length of <seq>) ;
arguments :change :ins/sup :uncom and :test for tuning the editing distance (see dist-edit)."))

(defmethod motif-list ((seq t) (out t)
		       &key (diss 0) (l-var 0) (n nil)
		       (change 1) (insert 1) (delete 1) (uncom 0) (test #'equalp))
  (when (not out) (setf out :length))
  (unless (member out '(:length :freq))
    (error "motif-list: unknown output ~S, use :length or :freq." out))
  (fv-morphologie::find-self
   seq out diss l-var n
   ;; the default distance (nil) gives the same results faster: with diss 0 and positive costs,
   ;; a distance of 0 means identical elements by test; with the default costs, it is the same
   ;; normalized edit distance
   (if (or (and (zerop diss) (plusp change) (plusp insert) (plusp delete) (not (minusp uncom)))
           (and (= change insert delete 1) (zerop uncom)))
       nil
       #'(lambda (a b) (edit-dist a b change insert delete uncom t test)))
   test))

(defgeneric dist-euclidian (a b)
  (:documentation "DIST-EUCLIDIAN: euclidian distance between points a and b.
Point coordinates are represented by a list of values in any euclidian space with n dimensions (n integer > 0).
The dimensionality of the euclidian space is defined by the list representing <a> and <b>.
 If a is a list of coordinates (lists) and b null,
output is the list of distances for all points to all others ;
 if a or b is a number, consider b or a as list of numbers (1 dim).
If a and b are lists with not same size (i.e points with not same dimensions),
 returns NIL (no distance)."))

(defmethod dist-euclidian ((a t) (b t))
  (fv-morphologie::dist-euclid a b))

(defgeneric dist-citybloc (a b)
  (:documentation "DIST-CITYBLOC: city-bloc distance between <x> and <y> which represent 'points' into kind of euclidian space.
If <y> is null, <x> may represent a list of points, then the output is the list of city-bloc distances for all points to all others."))

(defmethod dist-citybloc ((a t) (b t))
  (fv-morphologie::city-bloc a b))

(defgeneric dist-hamming (a b &optional norm test)
  (:documentation "DIST-HAMMING: Hamming distance between list a and list b."))

(defmethod dist-hamming ((a t) (b t) &optional (norm t) (test #'eq))
  (fv-morphologie::hamming-dist a b norm test))

(defgeneric dist-edit (seq1 seq2 &key sub ins del uncom norm test)
  (:documentation "DIST-EDIT: editing distance from seq1 to seq2.
Keywords arguments are:
:sub : cost for substituting symbols (1 by default)
:ins : cost for inserting symbols (1 by default)
:del : cost for deleting symbols (1 by default)
:uncom : cost for substituting symbols which are not common to both sequences
:norm : result divided by the length of the longer sequence, from 0 to 1 with costs of 1 (T, or NIL by default)
:test : identity function"))

(defmethod dist-edit ((seq1 t) (seq2 t)
		      &key (sub 1) (ins 1) (del 1) (uncom 0) (norm NIL) (test #'equalp))
  (fv-morphologie::edit-dist seq1 seq2 sub ins del uncom norm test))

(defgeneric dist-multi-edit (seq1 seq2 wgth &key sub ins del uncom test)
  (:documentation "DIST-MULTI-EDIT: edit distance between seq1 and seq2, whose elements are lists of descriptors (one per dimension),
the distances of the dimensions being weighted by wgth: a number (same weight for all) or a list of weights, one per dimension
(a list too short is completed with its last weight, a list too long is cut). The weights are normalized to sum to 1."))

(defmethod dist-multi-edit ((seq1 t) (seq2 t) (wgth t) &key (sub 1) (ins 1) (del 1) (uncom 0) (test #'equalp))
  (fv-morphologie::multi-edit-dist seq1 seq2 wgth :change sub :insert ins :delete del :inex uncom :test test))

(defgeneric dist-structure  (a b &key w-occ w-rep test)
  (:documentation "DIST-STRUCTURE: dissimilarity between the sequences <a> and <b> from the point of view of their structure,
from 0 (same structure) to 1: for each element, its number of occurrences and the position of its first occurrence
are compared with the edit distance. Experimental.
:w-occ, :w-rep: weights of the occurrences and of the repetitions; :test: identity of the elements (equalp by default).
If <b> is nil, the distances between all the sequences of <a>."))

(defmethod dist-structure  ((a t) (b t) &key (w-occ 1.) (w-rep 1.) (test #'equalp))
  (fv-morphologie::struct-dist a b w-occ w-rep test))

(defgeneric dist-graph (v1 v2 graph)
  (:documentation "DIST-GRAPH: distance between the nodes <v1> and <v2> of a graph: the length of the shortest path
between them (Dijkstra, see graph-path), 0 from a node to itself, nil if there is no path."))

(defmethod dist-graph ((v1 t) (v2 t) (graph t))
  (fv-morphologie::tree-dist v1 v2 graph))

(defgeneric histogram (data &key test thes)
  (:documentation "HISTOGRAM: histogram of data, as a list of (element count): sorted by increasing value for numbers,
else by decreasing count (elements of equal count in their order of first appearance). With optional arguments:
test: identity test (equalp by default)
thes: thesaurus or list of symbols to be considered."))

(defmethod histogram ((data t) &key (test #'equalp) (thes nil))
  (fv-morphologie::histogramme data :test test :thes thes))

(defgeneric entropy (data &optional mode samples test)
  (:documentation "ENTROPY: entropy of the set or sequence <data>, according to <mode> (a symbol of any package, a keyword or a string):
shannon-2 (or nil, default): Shannon entropy in base 2;
shannon-n: Shannon entropy in base n, the number of distinct elements (from 0 to 1);
shannon-e: Shannon entropy with the natural logarithm;
cond-sh: conditional entropy (experimental).
<samples>: number of consecutive windows of <data>, giving the list of their entropies.
<test>: identity of the elements (equalp by default)."))

(defmethod entropy ((data t) &optional (mode nil) (samples nil) (test #'equalp))
  ;; modes are recognized by their names: 'shannon-2 from any package, :shannon-2 or "shannon-2"
  ;; (they were compared with eq to the symbols of fv-morphologie)
  (let* ((name (if mode (string-upcase (string mode)) "SHANNON-2"))
         (r (cond ((string= name "SHANNON-2") (shannon-entropy data 2 samples test))
                  ((string= name "SHANNON-E") (shannon-entropy data (exp 1) samples test))
                  ((string= name "SHANNON-N") (shannon-entropy data nil samples test))
                  ((string= name "COND-SH") (entropie-conditionnelle data nil samples test))
                  (t (error "entropy: unknown mode ~S, use shannon-2, shannon-n, shannon-e or cond-sh." mode)))))
    (if (listp r) (mapcar #'float r) (float r))))

(defgeneric elt-info (data elt &optional test)
  (:documentation "ELT-INFO: self-information of the element <elt>: -log2 of its frequency in <data>, nil if the element
is absent or <data> is empty. With <elt> nil, each element with its information, the most informative first
(elements of equal information in their order of first appearance). <test>: identity of the elements (equalp by default)."))

(defmethod elt-info ((data t) (elt t) &optional (test #'equalp))
  (fv-morphologie::self-info data elt test))

;(defmethod redundancy ((data t))
;  :non-generic t
;  :class morphologie-box
;  (fv-morphologie::redondance data))

(defgeneric inner-dynamic (seq &optional test)
  (:documentation "INNER-DYNAMIC: a kind of inner dynamic of the distribution of the symbols (marks) of the sequence <seq>,
 one value for each element, computed by integrating the contrast/mark analysis: novelty of the marks and their echoes
 in the sequence (rewritten from Paolo Aralla's 'energy profile'). Experimental.
<test>: identity of the elements (equalp by default).
Reference: Paolo Aralla, 'Analisi morfologica. Un modello matematico per descrivere la relazione fra struttura morfologica
del messaggio, attivita' mnemonico-percettiva e risposta psichica', Prisma 01, Euresis Edizioni, 2002
(sections 3 'New/Old-Analysis' and 5 'Energy-profile')."))

(defmethod  inner-dynamic ((seq t) &optional (test #'equalp))
  (fv-morphologie::mark-dynamic seq test))

(defgeneric graph-length (graph)
  (:documentation "GRAPH-LENGTH: returns the (minimum) length of a graph (the graph must have no cycles, i.e. to a tree."))

(defmethod graph-length ((graph list))
  (fv-morphologie::tree-minlen graph))

(defgeneric graph-degree (node graph)
  (:documentation "GRAPH-DEGREE: degree of a node in a graph, the number of its edges (0 if it is not in the graph).
A list of nodes gives the list of their degrees, nil the degrees of all the nodes, in the order of graph-nodes."))

(defmethod graph-degree ((node t) (graph list))
  (fv-morphologie::tree-deg node graph))

(defgeneric graph-nodes (graph)
  (:documentation "GRAPH-NODES: list of all nodes of a graph, each node once, sorted."))

(defmethod graph-nodes ((graph list))
  (fv-morphologie::all-vertices graph))

(defgeneric graph-extrem (graph)
  (:documentation "GRAPH-EXTREM: returns all extremities (leaves) of a graph."))

(defmethod graph-extrem ((graph list))
  (fv-morphologie::tree-leaves graph))

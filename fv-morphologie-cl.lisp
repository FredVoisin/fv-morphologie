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

;;;; common-lisp fv-morphologie main functions (menu)
;;;; copyright (c) Frederic Voisin, 1992-2012
;;;; Morphologie tool suite (Library) for Common-Lisp, OpenMusic (Ircam, 1997) and PWGL (Sibelius Academy, 2007).
;;;; First imagined and developped with Herve Riviere at LACITO-CNRS (1992) using Patchwork (Ircam 1991), most of these functions were originaly published with Jacopo Baboni-Schilingi (Ircam, 1997) - most of them are actually largely modified following some new experiences in music analysis, composition and teaching (cf. Voisin 2015, Vincenot). 
;;;; Common-Lisp
;;;; UTF-8

;; load with ASDF: (asdf:load-system :fv-morphologie)

(in-package :fv-morphologie)

;;; FV-MORPHOLOGIE COMMON LISP MENU

;;;;;;;;;;;;
;;; 1. ENCODAGE
;;; 1.1 TO TRANSCODE

(defgeneric exsample (seq method mode thres &optional option)
  (:documentation "EXSAMPLE: samples a sequence of numerical values according to its signature (with its first and last values).
Available methods are (see int-signature for documentation):
- :minmax
- :minflexmax (default, or nil)
- :major-extrema
- :landmarks
Modes may be :values (default, or nil), :positions, :all or :resampled"))

(defmethod exsample ((seq list) (method symbol) (mode symbol) (thres t) &optional (option nil))
  ;; thres nil: no threshold, as 0 (no method applied to nil)
  (if seq
      (echantillonage seq (or method :minflexmax) (or mode :values) (or thres 0) option)
      nil))

(defgeneric split (seq &optional marks)
  (:documentation "SPLIT: splits <seq> according to marks, which are not kept, and returns the list of its segments.
A string is split at the characters of the string <marks> and at blanks.
A list is split at the elements of the list <marks> (compared with equalp), at a single mark, or where the predicate <marks> is true."))

(defmethod split ((seq list) &optional (marks nil))
  (split-list-using-marks seq marks))

(defmethod split ((seq string) &optional (marks " "))
  (split-string seq marks))

(defgeneric num>base (num base)
  (:documentation "NUM>BASE: converts an number to its representation in base <base>."))

(defmethod num>base ((num number) (base t))
  (fv-morphologie::num-base num base))

(defmethod list>sym ((list t))
  (fv-morphologie::concaten list))

(defgeneric graph>matrix (graph)
  (:documentation "GRAPH>MATRIX: the symmetric adjacency matrix of <graph>, a list of edges (node-a node-b distance),
as a list of rows: rows and columns in the order of graph-nodes, the distance of each edge, 0 elsewhere."))

(defmethod graph>matrix ((list t))
  (fv-morphologie::graph2matrix list))

;;; 1.2 PREPROCESSING

(defgeneric filt-fct (seq func wind &optional step)
  (:documentation "Apply function <func> to sequence <seq> thru a sliding window of length <wind> by (optional) step <step> (1 by default).
Window size may be an integer (samples) or a fraction of the length of seq; step size may be an integer (samples) or a fraction of the window.
With step 1 the window is centred on each value; with a larger step it is centred on each block of step values.
A value returned by <func> is repeated for the block; a list replaces the block (step = wind), or gives the values of the block's positions."))

(defmethod filt-fct ((seq list) (func function) (wind real) &optional (step 1))
  (let ((wind (if (integerp wind) wind (max 1 (round (* wind (length seq)))))))
    (fv-morphologie::window-apply seq func wind
                                  (if (integerp step) step (max 1 (round (* step wind)))))))

(defgeneric filt-mean (seq wind &optional step)
  (:documentation "Values replaced by their respective  windowed mean values."))

(defmethod filt-mean ((seq list) (wind real) &optional (step 1))
  (filt-fct seq #'fv-morphologie::mean wind step))

;(filt-mean '(1 2 1 1 3 4 5 6 0 0 1) 3)


(defmethod filt-median ((seq t) (window number))
  "FILT-MEDIAN: median filter."
  (fv-morphologie::median-filter seq window))

(defmethod filt-lowpass ((seq t) (alpha number) &optional (gamma nil))
  "FILT-LOWPASS: exponential filter"
  (if (not gamma)
      (fv-morphologie::exponential-smoothing seq alpha)
    (fv-morphologie::double-exponential-smoothing seq alpha gamma)))

(defmethod filt-local-rep ((seq t) &optional (mode nil) (test #'equalp))
  "FILT-LOCAL-REP: remove consecutive identical values or symbols (mode nil or :delete),
or, for numbers, replace them by a linear interpolation (mode :linear, or :interp as in PWGL)."
  (case mode
    ((nil :delete) (fv-morphologie::rem-local-rep seq test))
    ((:linear :interp)
     (if (every #'numberp seq)
         (fv-morphologie::nocons= seq test)
         (fv-morphologie::rem-local-rep seq test)))
    (t (error "filt-local-rep: unknown mode ~S, use nil, :delete or :linear." mode))))

(defgeneric filt-noise (seq &optional mode val)
  (:documentation  "FILT-NOISE: jitters or adds some noise into sequence <seq> using different methods, according to the first optional argument <mode>,
with the amount <val> (0.1 by default), relative to the range (max - min) of <seq>:
 :nozero : replaces each zero value of <seq> with a random value in ]0, val * range[
 :swap : swaps val * length pairs of elements (val integer: the number of swaps)
 :relative : adds to each value a random amount in [-val * range, val * range]
A sequence which is not numerical is swapped.
<mode> may also be a function of two arguments (x l): each element x of <seq> is replaced by its result, l being the whole <seq>; <val> is ignored.
For instance (lambda (x l) (declare (ignore x)) (nth (random (length l)) l)) replaces each element by a random element of <seq>."))

(defmethod filt-noise ((seq t) &optional (mode :nozero) (val 0.1))
  (fv-morphologie::fnoise seq mode val))

;;;;;;;;;;;;
;;; 2. DIFFERENTIATION
;;; 2.1 WITH INTENSITIES

(defgeneric int-signature (seq mode thres &optional option)
  (:documentation "INT-SIGNATURE: signature of a sequence with some threshold <thres> for variation in intensity.
The argument <thres> may be nil or 0 (no threshold for variations in intensity), or an absolute amplitude (in the unit of the values of <seq>): variations smaller than <thres> are ignored.
 Argument <mode> define the algorithm to compute signature which may require the optional argument <option> :
:miflexnmax, returns minima, maxima and inflexions using Fred Voisin pragmatic method.
:minmax, same with only maxima and minima (without inflexions);
:major-extrema, important extrema (Fink & Gandhi, Conference on Systems, Man and Cybernetics, 2007):
 a minimum (maximum) such that the sequence rises (falls) by at least thres on both sides;
:landmarks, first-order landmarks smoothed by MDPP(D, P) (Perng, Parker & Leung, Conference on Information
 and Knowledge Management, 1999): pairs of close landmarks whose difference is below thres are removed.
Argument <thres> is the treshold value (delta) in amplitude.
Argument <option> is an optional parameter depending to the choosen method :
with :minflexmax method, <option> is the time (rank) distance threshold for detecting inflexions (a nul or positive number);
with :major-extrema, <option> may be a distance function (a b) for comparing values (|a - b| by default);
with :landmarks, <option> is nil, a number D (only landmarks closer than D ranks may be removed), a function (a b) giving the
difference between two values (|a - b| by default), or a list (D criterion), criterion being :absolute, :relative
(2 |a - b| / (|a| + |b|), as in Perng et al.) or a function."))

(defmethod int-signature ((seq t) (mode t) (thres t) &optional (option nil))
  ;; thres nil: no threshold, as 0 (no method applied to nil)
  (when (not mode) (setf mode :minflexmax))
  (signature seq mode (or thres 0) option))

;;; 2.2 USING MARKS

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

(defgeneric mark-structure  (seq out &key diss rem-loc-dup test)
  (:documentation "MARK-STRUCTURE: returns possible structures of input <seq> based on marks retrieved from <seq>.
The output is sorted by structures's length.
Use output menu <out> to define result form (another output is an error):
:struct (or nil) lists all possible structures according to different marks of <seq>
:pos each structure is consed to the list of positions for each segment followed by the list of the segments themselves
:raw each structure is consed to the list of the different segments, each followed by all its positions in <seq>.
Option :diss defines the dissemblance threshold to consider different segment as the same.
Option :test (equalp by default) defines the identity of elements: of the marks, of the elements of the segments
compared by the edit distance, and of the consecutive elements removed by :rem-loc-dup.
Option :rem-loc-dup (t by default) removes the local repetitions inside each segment; positions refer to <seq>."))

(defmethod mark-structure  ((seq t) (out t) &key (diss 0) (rem-loc-dup t) (test #'equalp))
  (when (not out) (setf out :struct))
  (mark-strct seq out diss rem-loc-dup test))

;;; 2.3 USING MOTIFS

(defgeneric motif-find (motif seq &key diss l-var change ins del uncom test)
  (:documentation "MOTIF-FIND: returns all positions of a <motif> into a sequence <seq>.
Argument :diss is the threshold (from 0 to 1.0) of dissimilarity according to the normalized editing distance under which two different segments are considered to be the same ;
argument :l-var is the threshold for variation in length of the motifs to be compared ;
arguments :change :ins/sup :uncom and :test for tuning the editing distance (see dist-edit)."))

(defmethod motif-find ((motif t) (seq t) &key (diss 0) (l-var 0) (change 1) (ins 1) (del 1) (uncom 0) (test #'equalp))
  (find-pos motif seq diss l-var
            #'(lambda (a b) (edit-dist a b change ins del uncom t test))))

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

(defgeneric motif-structure (seq &key lengths test diss l-var criteria distance)
  (:documentation "MOTIF-STRUCTURE: structure of <seq> in terms of its repeated motifs, as a list of two elements:
the positions where motifs begin, each followed by the numbers of the motifs beginning there, then the motifs.
A motif is a segment repeated at least once, later and without overlapping.
:lengths the lengths of the motifs searched: (min max), or a maximum k for lengths 2 to k (default: 3 to half the length of <seq>);
:test the identity of elements (equalp by default);
:diss the tolerance, the maximum normalized edit distance (0 to 1) between a motif and its repetitions (0 by default: exact repetitions);
:l-var the length variation of the repetitions, a fraction of the length of the motif (0 by default; used with a tolerance);
:criteria :length (default) or :freq, to keep the longest or the most frequent of the motifs contained in one another;
:distance a function (a b) giving the distance between two segments (lists), instead of the edit distance; :diss is then in its unit."))

(defmethod motif-structure ((seq t) &key (lengths nil) (test #'equalp) (diss 0) (l-var 0) (criteria :length) (distance nil))
  (unless (member criteria '(:length :freq))
    (error "motif-structure: :criteria must be :length or :freq, not ~S." criteria))
  (unless (or (null lengths) (and (integerp lengths) (plusp lengths))
              (and (listp lengths) (= 2 (length lengths)) (every #'integerp lengths)
                   (<= 1 (first lengths) (second lengths))))
    (error "motif-structure: :lengths must be (min max) or a maximum length, not ~S." lengths))
  (fv-morphologie::rep-strct seq :diss diss :l-var l-var :n lengths :criteria criteria
                                 :distance-fct distance :test test))

(defgeneric motif-group (seq test &optional key)
  (:documentation "MOTIF-GROUP: assembles consecutive elements or motifs of <seq> for which the comparison <test> (by default: equalp) is successful.
Optional argument <key> defines a function to be applyed to each consecutive element or motif to be compared."))

(defmethod motif-group ((seq t) (test function) &optional (key nil))
  (adj-group seq test key))

;;; 2.4 CLASSIFICATION (ANOTHER KIND OF DIFFERENTIATION)

(defgeneric class-num (data classes mode  &key iter dist)
  (:documentation "CLASS-NUM: classify a set (list) of points <data> into different <classes> into s space with n dimensions (n integer > 0),
where each point is represented as a list of values into each dimension.
Argument :mode defines the algorithm used for partioning the points.
:centroids for centroids algorithm (in french : nuees dynamiques.
Keyword argument :iter to set the maximum iterations to run for partitioning ;
:dist to set the distance or metric to be used (by default: euclidian distance)."))

(defmethod class-num ((data t) (classes integer) (mode symbol) &key (iter nil) (dist nil))
  (setf classes (max 2 classes))
  (case mode
    ((nil :centroids) (fv-morphologie::n-class data classes iter dist))
    (:1d-centroids (fv-morphologie::1d-class data classes (if dist dist #'identity))) 
     (t nil)))

(defgeneric class-sym (data classes mode &key uncom ins del change excluded mst)
  (:documentation "CLASS-SYM: classify a set of segments or sequences into different classes according to their editing distance to each other and
by partitioning the resulting spanning tree (see class-graph).
The output is a list of numbers (from zero) as instances of the different classes retrieved.
Keyword arguments :uncom :ins/del :change for tuning editing distance (see dist-edit).
The argument :excluded is a list of elements of data excluded from classification ;
in the output, the numbering of the different instances begins by numbering these elements first (from zero), 
in the order defined with the argument :excluded."))

(defmethod class-sym ((data t) (classes integer) (mode symbol)
		      &key (uncom .5) (ins 1) (del 1) (change 1) (excluded nil) (mst nil))
  (setf classes (max 2 classes))
  (if (not excluded)
      (cond ((or (eq mode :edit-nn) (not mode))
             (fv-morphologie::s-class data classes nil change ins del uncom mst))
            ((eq mode :edit-norm)
             (fv-morphologie::s-class data classes t change ins del uncom mst))
            (t nil))
    (cond ((eq mode :edit-nn)
           (fv-morphologie::s-class-with-fixed data classes excluded nil nil nil change ins del uncom mst))
          ((eq mode :edit-norm)
           (fv-morphologie::s-class-with-fixed data classes excluded nil nil t change ins del uncom mst))
          (t nil))))

;; 2.5 CONCATENATION

;(defmethod concaten ((list nil))
;  :non-generic t
;  :class morphologie-box
;  (fv-morphologie::concaten list))

;(defmethod adj-group ((seq nil) (test #'eq) &optional (key nil))
;  :non-generic t
;  :class morphologie-box
;  (fv-morphologie::adj-group seq test key))

;;;;;;;;;;;;
;;; 3. EVALUATION
;;; 3.1 DISTANCES AND DISSIMILARITIES

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
:ins : cost for inserting symbols (1 by default 
:del : cost for deleting symbols (1 by default)
:uncom : cost for substituting symbols which are not common to both sequences
:norm : result normalized to the length of the shorter sequence (T or NIL by default)
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
  (:documentation "DIST-STRUCTURE: "))

(defmethod dist-structure  ((a t) (b t) &key (w-occ 1.) (w-rep 1.) (test #'equalp))
  (fv-morphologie::struct-dist a b w-occ w-rep test))

(defgeneric dist-graph (v1 v2 graph)
  (:documentation "DIST-GRAPH: "))

(defmethod dist-graph ((v1 t) (v2 t) (graph t))
  (fv-morphologie::tree-dist v1 v2 graph))

;;; 3.2 INFORMATION (DYNAMICS ?)

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
  (:documentation "ELT-INFO:"))

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
<test>: identity of the elements (equalp by default)."))

(defmethod  inner-dynamic ((seq t) &optional (test #'equalp))
  (fv-morphologie::mark-dynamic seq test))

;;;;;;;;;;;;
;;; 4. DELINEATION
;;; 4.1 BUILDING GRAPHS OR TREES

(defgeneric graph-span (mat-dist &optional verbose)
  (:documentation   "GRAPH-SPAN: minimum spanning tree build on a semi-matrix of distances <mat-dist>."))

(defmethod graph-span ((mat-dist list) &optional (verbose nil))
  (minimum-spanning-tree mat-dist verbose))

(defgeneric graph-part (graph &optional mode n)
  (:documentation  "GRAPH-PART: partition a graph according to edges which have maximum length.
Without <n>, all the edges of maximum length are cut ; with <n>, the n-1 longest edges are cut, giving n parts."))

(defmethod graph-part ((graph list) &optional (mode :distance) (n nil))
  (part-graph graph mode n))

;;; 4.2 MEASURING

(defgeneric graph-path (from to graph)
  (:documentation "GRAPH-PATH: returns path(s) from node(s) or leave(s) to one or some other node(s) or leave(s) of a graph (the graph must have np cycles, i.e. to be a tree."))

(defmethod graph-path ((from t) (to t) (graph list))
  (fv-morphologie::tree-path from to graph))

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

;;; 4.3 DRAW

(defgeneric graph>dot (graph out &key dis scale shape legend)
  (:documentation   "GRAPH>DOT: converts a graph (list of edges) into an undirected graph into dot langage to be processed using neato program.
Arguments:
<graph> : the graph, as a structured list.
<out> : if NIL or T, print out in listener; if a string, it defines the pathname of the output. Type of file define output.
Keywords:
 :dis : amount of distortion allowed for distances fir drawing the graph (from 0.0 for minimum distorsion to 1.0 maximum distorsion)
 :scale : scaling factor for the mininum distance
 :shape : string or list of strings to define node's shape, using 'neato' definitions
"))

(defmethod graph>dot  ((graph t) (out t) &key (dis 1) (scale t) (shape "ellipse") (legend t))
  (fv-morphologie::graph2dot graph dis scale shape out legend))

;;;;;;;;;;;;
;;; 5. IMPORT/EXPORT
;;; 5.1 READING FILES

(defgeneric read-text (file &key mode sep rem-test)
  (:documentation "READ-TEXT: reads text <file> using following <mode> formats (another mode is an error):
 T : each line of file as a list of symbols or numbers
 NIL : flat mode, reads file as a single list of strings
 :charcode : the segments of the file, as lists of character codes.
Additional keyword :sep is a string defining all characters defining a separator, besides blanks
(spaces, tabulations, newlines), or t for the usual punctuation marks; with :charcode, it may also
be a list of characters or codes, or (:ignore c ...) to separate at all the characters which are
not letters or digits (in any alphabet), but c ...
Additional Keyword :rem-test defines a function to remove choosen words."))

(defmethod read-text ((file t)  &key (mode t) (sep nil) (rem-test nil))
  (read-txt-file file :mode mode :marks sep :rem-test rem-test))

; (defmethod read-midi ((file))
; (defmethod read-sdif ((file))

;;; 5.2 WRITING FILES

(defgeneric display-list (list &optional recursive)
  (:documentation "DISPLAY-LIST: prints out to the Lisp listener each element of list within a newline with indentation relative to list level."))

(defmethod display-list (list &optional (recursive nil))
  (list-display list))
  
(defgeneric write-list (list file &optional mode)
  (:documentation "WRITE-LIST: write list to a file using following optional argument <mode> format:
:flat (by default) : each element of list is written in a new line, sub-lists without parentheses (as read by read-text)
:lisp : list is written as a Lisp list
:coll : a Max coll file, one line index, data; for each element, from index 0."))

(defmethod write-list ((list list) (file t) &optional (mode :supersede))
  (list-write list file mode))

;;;;;;;;;;;;
;;; 6. DOCUMENTATION

(defun generic-function-p (self)
  "T if self is a class of <standard-generic-function>."
  (or (eq (class-name
           (class-of self))
          'standard-generic-function)
      (eq (class-name
       (class-of self))
      'compiled-function)))

(defun find-functions (&optional (package (find-package 'fv-morphologie)))
  "Give the list of generic functions in <package>."
  (when (symbolp package) (setf package (find-package package)))
  (let
    ((func '()))
    (do-symbols (sym package)
      (when (eq package (symbol-package sym))
        (let ((value (when (fboundp sym) (symbol-function sym))))
          (when (and value
		     (or ;(compiled-function-p value)
			 ;(function-p value)
			 (generic-function-p value)))
            (push sym func)))))
    (sort func #'string-lessp)))

;(print (find-functions))

(defparameter *fv-morphologie-menu* '(
int-signature
mark-position
mark-list
mark-structure
motif-find
motif-list
motif-structure
motif-group
class-num
class-sym
dist-euclidian
dist-citybloc
dist-hamming
dist-edit
dist-multi-edit
dist-structure
dist-graph
histogram
entropy
elt-info
inner-dynamic
graph-span
graph-part
graph-path
graph-length
graph-degree
graph-nodes
graph-extrem
graph>dot
exsample
split
))


(defgeneric doc (&optional fct)
  (:documentation
  "DOC: Prints out the documentation of some fv-morphologie function(s).
If no argument, prints out the list of all generic functions.
If argument is a symbol or a list of symbols, prints out documentation of the corresponding functions.
Exemples: 
(doc)
(doc 'dist-edit)"))

(defmethod doc (&optional (fct nil))
  (cond
    ((not fct)
     (format t "~%fv-morphologie generic functions :")
     (dolist (d (doc 'list) (format t "~%")) (format t "~% ~(~S~)" d)))
    ((listp fct) (dolist (f fct) (format t "~S~%~%" (documentation f 'function))))
    ((symbolp fct) (case fct
		     (list *fv-morphologie-menu*)
		     (all (dolist (d (doc 'list) (format t "~%")) (format t "~S~%~%" (documentation d 'function))))
		     (t (documentation fct 'function))))
    (t (doc 'all))))

(defparameter *fv-morphologie-doc-list*
  (remove-if-not #'listp
		 (loop for doc in (mapcar (lambda (f) (documentation f 'function)) (doc 'list))
		    collect (remove-if #'(lambda (x) (< (length x) 4))
				       (split-string doc "(<>) ,;.!:?'\"")))))

(defgeneric help (&optional keyword threshold)
  (:documentation "HELP: look for a keyword in fv-morphologie the documentation of fv-morphologie generic functions and prints out the documentation retrieved.
If no argument (keyword), prints out documentation of all generic functions."))

(defmethod help (&optional (keyword nil) (threshold .5))
  (if (not keyword)
      (doc)
      (let ((klen (length (string keyword)))
	    matches tmp)
	(setf matches
	      (dolist (fctdoc *fv-morphologie-doc-list*)
		(let ((p))
		  (setf tmp ;(remove-if #'(lambda (x) (< x threshold))
				       (mapcar (lambda (d) (list (- 1 (dist-edit (read-from-string d)
										 keyword :norm T)) d))
					       fctdoc)
			);	       :key #'car))
		  (when tmp
		    (print tmp))))))))

;(help)
;(help 'graph)

;(doc)
;(doc 'list)
;(doc 'all)
;(doc 'dist-edit)

(format t "~%FV-MORPHOLOGIE loaded : for help, in package :fv-morphologie type (HELP)~%")
;eof


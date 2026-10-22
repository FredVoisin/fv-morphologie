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

;;;; api/transcription.lisp
;;;;
;;;; Public functions: transcription (encoding, filters) and graphs
;;;; (spanning tree, paths, matrices, DOT).
;;;;
;;;; copyright (c) Frederic Voisin, 1992-2012
;;;; Morphologie tool suite (Library) for Common-Lisp, OpenMusic (Ircam, 1997) and PWGL (Sibelius Academy, 2007).
;;;; First imagined and developped with Herve Riviere at LACITO-CNRS (1992) using Patchwork (Ircam 1991), most of these functions were originaly published with Jacopo Baboni-Schilingi (Ircam, 1997) - most of them are actually largely modified following some new experiences in music analysis, composition and teaching (cf. Voisin 2015, Vincenot). 
;;;; Common-Lisp
;;;; UTF-8


(in-package :fv-morphologie)

(defgeneric num>base (num base)
  (:documentation "NUM>BASE: converts an number to its representation in base <base>."))

(defmethod num>base ((num number) (base t))
  (fv-morphologie::num-base num base))

(defgeneric list>sym (list)
  (:documentation "LIST>SYM: concatenates the elements of <list> into one element: numbers into a number,
strings into a string, other elements into a symbol."))

(defmethod list>sym ((list t))
  (fv-morphologie::concaten list))

(defgeneric graph>matrix (graph)
  (:documentation "GRAPH>MATRIX: the symmetric adjacency matrix of <graph>, a list of edges (node-a node-b distance),
as a list of rows: rows and columns in the order of graph-nodes, the distance of each edge, 0 elsewhere."))

(defmethod graph>matrix ((list t))
  (fv-morphologie::graph2matrix list))

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


(defgeneric filt-median (seq window)
  (:documentation "FILT-MEDIAN: median filter over a window of <window> values centred on each value of <seq>
(an even window is enlarged to the next odd size, so that the result is made of values of <seq>);
the first and last values are repeated at the edges. The result has the same length as <seq>."))

(defmethod filt-median ((seq t) (window number))
  (fv-morphologie::median-filter seq window))

(defgeneric filt-lowpass (seq alpha &optional gamma)
  (:documentation "FILT-LOWPASS: exponential smoothing of <seq>: each value moves by the fraction <alpha> (0 to 1) towards
the input, s(t) = s(t-1) + alpha (x(t) - s(t-1)). With <gamma> (0 to 1), double exponential smoothing (Holt), which also
follows the trend of <seq>. The result has the same length as <seq>."))

(defmethod filt-lowpass ((seq t) (alpha number) &optional (gamma nil))
  (if (not gamma)
      (fv-morphologie::exponential-smoothing seq alpha)
    (fv-morphologie::double-exponential-smoothing seq alpha gamma)))

(defgeneric filt-local-rep (seq &optional mode test)
  (:documentation "FILT-LOCAL-REP: removes the consecutive identical elements of <seq> (<mode> nil or :delete), or, for
numbers, replaces each run of identical values by a ramp towards the next different value (<mode> :linear, or :interp
as in PWGL), keeping the length of <seq>. <test>: identity of the elements (equalp by default)."))

(defmethod filt-local-rep ((seq t) &optional (mode nil) (test #'equalp))
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

(defgeneric graph-span (mat-dist &optional verbose)
  (:documentation   "GRAPH-SPAN: minimum spanning tree build on a semi-matrix of distances <mat-dist>."))

(defmethod graph-span ((mat-dist list) &optional (verbose nil))
  (minimum-spanning-tree mat-dist verbose))

(defgeneric graph-path (from to graph)
  (:documentation "GRAPH-PATH: returns path(s) from node(s) or leave(s) to one or some other node(s) or leave(s) of a graph (the graph must have np cycles, i.e. to be a tree."))

(defmethod graph-path ((from t) (to t) (graph list))
  (fv-morphologie::tree-path from to graph))

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

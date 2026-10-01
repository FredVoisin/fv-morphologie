;;; fv-morphologie PWGL specific file
;;; copyright (c) Frederic Voisin, 1992-2013
;;; Requires OMPW package (ASDF)

; next 15 lines for accentuated characters in tutorial, thank's to Mika
(in-package :dbl)
(defmethod parse-DBL-doc ((pathname pathname) &key master-db-document document-source backend capi-args panes-only-p)
 (declare (ignore document-source))
 (if (equalp (pathname-type pathname) "DBD")
     (let ((*db-directory* pathname))
       (with-open-stream (stream (open pathname :direction :input
				       :element-type 'ccl::simple-char 
				       :external-format :utf-8))
         (parse-DBL-doc-stream stream :master-db-document
			       master-db-document :document-source
			       pathname :backend backend 
			       :capi-args capi-args
			       :panes-only-p panes-only-p)))
   (error "Not a DBD file")))

;;;;;;;;;;;;;;;;
;; Boxes & menu

(in-package :fv-morphologie)
(defclass  morphologie-box  (ccl::PWGL-box) ())
(defmethod ccl::special-info-string ((self morphologie-box)) "fv-morph")
(ccl::write-key 'morphologie-box  :code-compile t)

(define-menu fv-morphologie :print-name "fv-morphologie")
(in-menu fv-morphologie)

;;;;;;;;;;;;;;;;
;;; 1. TRANSCRIPTION

(define-menu TRANSCRIPTION  :in fv-morphologie)
(in-menu TRANSCRIPTION)

;;  1.1 TRANSCODER : convert, encode

(define-box transcode ((seq nil) (table nil) &optional (test #'eq))
  :non-generic t
  :class morphologie-box
  (fv-morphologie::transcode seq table test))

(define-box num>base ((num 0) (base nil))
  :non-generic t
  :class morphologie-box
  (fv-morphologie::num-base num base))

(define-box num>alpha ((num 0))
  :non-generic t
  :class morphologie-box
  (fv-morphologie::num>alpha num))

(define-box alpha>num ((alpha abc) &optional (mode nil))
  :non-generic t
  :class morphologie-box
  :menu (mode (nil nil) (:midi ":midi"))
  (fv-morphologie::alpha>num alpha))

(define-box list>sym ((list nil))
  :non-generic t
  :class morphologie-box
  (fv-morphologie::concat list))

(define-box graph>matrix ((list nil))
  :non-generic t
  :class morphologie-box
  (fv-morphologie::graph2matrix list))

(define-box randomize ((seq nil) &optional (mode :nozero) (val 0.1))
  "Add noise into a sequence."
  :non-generic t
  :class morphologie-box
  :menu (mode (:nozero ":nozero") (:swap ":swap") (:relative ":relative"))
  (fv-morphologie::fnoise seq mode val))

;;  1.2 FILTRER : filter
(menu-separator :in TRANSCRIPTION)

(define-box filt-median ((seq nil) (window 3))
  "median filter"
  :non-generic t
  :class morphologie-box
  (fv-morphologie::median-filter seq window))

(define-box filt-exponential ((seq nil) (alpha .5) &optional (gamma nil))
  "Low-pass filter"
  :non-generic t
  :class morphologie-box
  (if (not gamma)
      (fv-morphologie::exponential-smoothing seq alpha)
    (fv-morphologie::double-exponential-smoothing seq alpha gamma)))

(define-box filt-extrema ((seq nil) (window nil))
  "Kind of inverse median filter : remains extremas"
  :non-generic t
  :class morphologie-box
  (fv-morphologie::extrema-filter seq window))

(define-box filt-local-rep ((seq nil) &optional (mode nil) (test equalp))
  "Delete consecutive or adjacent values or symbols which are identical according to test."
  :non-generic t
  :class morphologie-box
  :menu (mode (:delete ":delete") (:linear ":interp"))
  (cond ((eq mode :delete)
         (fv-morphologie::rem-local-rep seq test))
        ((eq mode :linear)
         (if (not (member 'nil (mapcar #'numberp seq)))
             (fv-morphologie::nocons= seq test)
           (filt-local-rep seq :delete test)))
        (t (filt-local-rep seq :delete test))))

;;  1.3 DELINEER : trace graphs
(menu-separator :in TRANSCRIPTION)

(define-box graph-span ((mat-dist nil))
  "Minimum spanning tree build on a semi-matrix of distances <mat-dist>."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::minimum-spanning-tree mat-dist))

(define-box graph-path ((from nil) (to nil) (tree nil))
  :documentation "Returns the path in a graph from a node (<from>) to another one (<to>). If argument <to> is nil, returns all the paths from the node <from>."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::tree-path from to tree))

(define-box graph>dot  ((graph nil) (distorsion 0) (scale nil) (out t) &key (shape "ellipse") (legend t))
  :documentation "Converts a graph (list of edges) into an undirected graph into dot langage to be processed using neato program.
Arguments are:
graph : the graph, as a structured list.
distorsion : amount of distortion allowed for distances fir drawing the graph (from 0.0 for minimum distorsion to 1.0 maximum distorsion)
scale : scaling factor for the mininum distance
shape : string or list of strings to define node's shape, using 'neato' definitions
out :
if NIL or T, print out in listener;
if a string, it defines the pathname of the output. The type of file defines output."
  :non-generic t
  :class morphologie-box
  ;:menu (shape ("ellipse" "ellipse") ("plaintext" "plaintext") ("point" "point"))
  (fv-morphologie::graphtodot graph distorsion scale out :shape shape :legend legend))

;;  1.4 INTERPOLER : interpolate
(menu-separator :in TRANSCRIPTION)

(define-box morph-lists ((seq1 list) (seq2 list))
  :documentation "Interpolates sequence seq1 to sequnece seq2 with minimum steps."
  (s-interpolate seq1 seq2))

;;;;;;;;;;;;;;;;
;;; 2. CLASSIFICATION
(define-menu CLASSIFICATION :in fv-morphologie)
(in-menu CLASSIFICATION)

;;  2.1 SEGMENTER : cut, split

; old version of int-landmarks for back compatibility
;(define-box int-primitives ((seq nil)  &optional (out :prim) (thres nil))
;  "Please update this box with int-landmarks"
;  :non-generic t
;  :class morphologie-box
;  :menu (out (:prim ":prim") (:pos ":pos") (:amp ":amp"))
;  (fv-morphologie::primitives seq out thres))

;(define-box int-sign ((seq nil) &key (pos nil) (thres 0))
;  "Sign of variation of intensities : -1 for decreasing, 1 for increasing, 0 for constant.
;Optional argument <pos>, if T returns each sign of variations consed to its position in sequence <seq>,
;or only signs of variations if NIL.
;Optional argument <thres> is the threshold for detecting only variations higher than <thres> value,
;the smaller being considered as constant (zro)."
;  :non-generic t
;  :class morphologie-box
;  (fv-morphologie::dsign seq thres pos))

(define-box signature ((seq nil) (mode :minflexmax) (thres 0) &optional (option nil))
  "Signature of a sequence of numeral values with some threshold <thres> for variation in intensity.
The argument <thres> may be nil (no threshold for variations in intensity), or an amount of the overall range of variation in the sequence <seq>, from 0.0 to 1.0 (where 0.0 means no threshold and 1.0 to remove all variation but the first).
 Argument <mode> define the algorithm to compute signature which may require the optional argument <option> :
:miflexnmax, returns minima, maxima and inflexions using Fred Voisin pragmatic method.
:minmax, same with only maxima and minima (without inflexions);
:major-extrema, using Fink & Gandhi method (cf. Conference on Systems, Man and Cybernetics, 2007);
:landmarks, using Perng, Parker and Leung method for landmarks (cf. Conference on Information and Knowledge Management, 1999);
Argument <thres> is the treshold value (delta) in amplitude.
Argument <option> is an optional parameter depending to the choosen method :
with :minflexmax method, <option> is the time (rank) distance threshold for detecting inflexions (a nul or positive number);
with :major-extrema, <option> defines the distance for comparing local extrema ; 
with :landmarks method, <option> is the time (rank) distance threshold for detecting a major extrema."
  :non-generic t
  :class morphologie-box
  :menu (mode (:minflexmax ":minflexmax") (:minmax ":minmax") (:important-extrema ":extrema") (:mdpp ":landmarks"))
  (fv-morphologie::signature seq mode thres option))

(define-box exsample ((seq nil) (method :landmarks) (mode :untimed) (thres 0) &optional (option nil))
  :documentation "Samples a sequence of numerical values according to its signature."
  :non-generic t
  :class morphologie-box
  :menu (method (:minmax "minmax") (:minflexmax "minflexmax") (:landmarks ":landmarks") (:major-extrema "major-extrema"))
  :menu (mode (:untimed ":untimed") (:timed ":timed"))
  (if seq (echantillonage seq method mode thres option) nil))

(define-box mark-cut ((seq nil) (marks nil))
  "Cuts a seq according to marks."
  :non-generic t
  :class morphologie-box
  (if (and seq marks)
      (cond ((listp seq)
	     (split-list-using-marks seq marks))
	    ((stringp seq)
	     (split-string seq marks))
	    (t seq))
      seq))

(define-box graph-part ((graph nil) &optional (mode :distance))
  "Partition a graph. Experimental, work in progress."
  :non-generic t
  :class morphologie-box
  :menu (mode (:distance "distance"))
  (fv-morphologie::part-graph graph mode))


;;  2.2 CONCATENER : group
(menu-separator :in CLASSIFICATION)

(define-box group ((seq nil) &optional (test nil))
  "Concatenates adjacent lists. If optional argument test is T, concatenates only if some interesection exists between adajacent lists. Work in progress for various conditions..."
  :non-generic t
  :class morphologie-box
  (if (not test)
      (fv-morphologie::concaten seq)
      (regroup-if-intersection seq)))

(define-box motif-group ((seq nil) (test equalp) &optional (key nil))
  "Assembles consecutive elements or motifs of <seq> for which the comparison <test> (by default: equalp) is successful.
Optional argument <key> defines a function to be applyed to each consecutive element or motif to be compared."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::adj-group seq test key))

;;  2.3 DIFFERENCIER : classify, identify
(menu-separator :in CLASSIFICATION)

(define-box class-num ((data nil) (classes 2) (mode :centroids) &key (iter nil) (dist nil))
  "Classify a list of points <data> into different <classes> into s space with n dimensions (n integer > 0),
where each point is represented as a list of values into each dimension.
Argument :mode defines the algorithm used for partioning the points.
:centroids for centroids algorithm (in french : nuees dynamiques.
Keyword argument :iter to set the maximum iterations to run for partitioning ;
:dist to set the distance or metric to be used (by default: euclidian distance)."
  :non-generic t
  :class morphologie-box
  :menu (mode (:centroids "centroids") (:1d-centroids "1d-centroids"))
  (case mode 
    (:centroids (fv-morphologie::n-class data classes iter dist))
    (:1d-centroids (fv-morphologie::1d-class data classes (if dist dist #'identity))) 
     (t nil)))

(define-box class-sym ((data nil) (classes 2) (mode :edit-nn) &key (uncom .5) (ins 1) (del 1) (change 1) (excluded nil) (mst nil))
  "Classify a list of segments or sequences into different classes according to their editing distance to each other and by partitioning the resulting spanning tree (see class-graph).
The output is a list of numbers (from zero) as instances of the different classes retrieved.
Keyword arguments :uncom :ins/del :change for tuning editing distance (see dist-edit).
The argument :excluded is a list of elements of data excluded from classification ;
in the output, the numbering of the different instances begins by numbering these elements first (from zero), 
in the order defined with the argument :excluded."
  :non-generic t
  :class morphologie-box 
  :menu (mode (:edit-nn "edit-nn") (:edit-norm "edit-norm"))
  (if (not excluded)
      (cond ((eq mode :edit-nn)
             (fv-morphologie::s-class data classes nil change ins del uncom mst))
            ((eq mode :edit-norm)
             (fv-morphologie::s-class data classes t change ins del uncom mst))
            (t nil))
    (cond ((eq mode :edit-nn)
           (fv-morphologie::s-class-with-fixed data classes excluded nil nil nil change ins del uncom mst))
          ((eq mode :edit-norm)
           (fv-morphologie::s-class-with-fixed data classes excluded nil nil t change ins del uncom mst))
          (t nil))))

(define-box mark-structure  ((seq nil) (out :struct) &key (diss 0) (rem-loc-dup t) (test equalp))
  "Returns possible structures of input <seq> based on marks retrieved from <seq>.
The output is sorted by structures's length.
Use output menu <out> to define result form:
:struct lists all possible structures according to different marks of <seq>
:pos each structure is consed to the list of positions for each segment followed by the list of the segments themselves
:raw each structure is consed to the list of each segment with its positions in <seq>.
Option :diss defines the dissemblance threshold to consider different segment as the same.
Option :test defines the test function to consider the marks."
  :non-generic t
  :class morphologie-box
  :menu (out (:struct ":struct") (:pos ":pos") (:raw ":raw") )
  (fv-morphologie::mark-strct seq out diss rem-loc-dup test))

(define-box motif-structure ((seq nil))
  "To be set"
  :non-generic t
  :class morphologie-box
  (fv-morphologie::rep-strct seq))

;;;;;;;;;;;;;;;;
;;; 3. EVALUATION

(define-menu EVALUATION :in fv-morphologie)
(in-menu EVALUATION)

;;  3.1 ENUMERER : filter, sample, find, count 
(menu-separator :in EVALUATION)

(define-box exsample ((seq nil) (method :landmarks) (mode :untimed) (thres 0) &optional (option nil))
  :documentation "Samples a sequence of numerical values according to its signature."
  :non-generic t
  :class morphologie-box
  :menu (method (:minmax "minmax") (:minflexmax "minflexmax") (:landmarks ":landmarks") (:major-extrema "major-extrema"))
  :menu (mode (:untimed ":untimed") (:timed ":timed"))
  (if seq (echantillonage seq method mode thres option) nil))

(define-box mark-list ((seq nil) (mark nil) &key (mark-test equalp) (seg-test equalp))
  "Returns the list of all segments and their position in <seq> beginning with some marks defined with argument <mark>.
If no argument mark (nil), considers all different symbols in <seq>.
Option :mark-test defines the test function for searching the marks (equalp by default).
Option :seg-test defines the test function for comparing the different segments."
  :non-generic t
  :class morphologie-box
  (when (and (symbolp mark-test) (not (boundp mark-test))) (setf mark-t (eval `(function ,mark-test))))
  (when (and (symbolp seg-test) (not (boundp seg-test))) (setf group-t (eval `(function ,seg-test))))
  (fv-morphologie::mark-pos seq mark :mark-test mark-test :seg-test seg-test))

(define-box mark-position ((seq nil) (mark nil) &optional (test equalp) (cons t))
  "Returns all positions of marks in seq."
  :non-generic t
  :class morphologie-box
  (all-pos seq mark test cons))

(define-box motif-list ((seq nil) (out :length) &key (diss 0) (l-var 0) (n nil) (distance-fct equalp))
  "Returns the list of all motifs found into <seq> where a motif is any segment repeted at least one time according to editing distance.
Output can be sorted according to the length of segment (:length) or to their frequency (:freq).
Argument :diss is a threshold (from 0 to 1.0) of dissimilarity according to the normalized editing distance under which two different segments are considered to be the same ;
argument :l-var is the threshold for variation in length of the segments to be compared ;
argument :n is the maxinum length of segments to be compared ;
arguments :change :ins :sup :uncom and :test are arguments for the editing distance (see dist-edit)."
  :non-generic t
  :class morphologie-box
  :menu (out (:length ":length") (:freq ":freq"))
  (when (not n) (setf n (list 2 (floor (/ (length seq) 2)))))
  (fv-morphologie::find-self seq out diss l-var n distance-fct))

(define-box motif-position ((motif nil) (seq nil) &key (diss 0) (l-var 0) (change 1) (ins 1) (del 1) (uncom 0) (test equalp))
  "Returns all the positions of the segment <motif> into sequence <seq>.
Argument :diss is a threshold (from 0 to 1.0) of dissimilarity according to the normalized editing distance under which two different segments are considered to be the same ;
argument :l-var is the threshold for variation in length of the motifs to be compared ;
arguments :change :ins/sup :uncom and :test for tuning the editing distance (see dist-edit)."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::find-pos motif seq diss l-var change ins del uncom test))

(define-box graph-nodes ((graph nil) &optional (min-deg 2))
  :documentation "Returns nodes of a graph. Optional argument is the minimum degree of the node (by definition > 1)."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::tree-nodes graph min-deg))

(define-box graph-extrem ((graph nil))
  :documentation "Returns all extremities (or leaves) of a graph."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::tree-leaves graph))

;;  3.2 COMPARER : distances and dissimilarities
(menu-separator :in EVALUATION)

(define-box dist-euclidian ((a nil) (b nil) &optional (key nil))
  "Euclidian distance between points a and b.
Point coordinates are represented by a list of values in any euclidian space with n dimensions (n integer > 0).
The dimensionality of the euclidian space is defined by the list representing <a> and <b>.
 If a is a list of coordinates (lists) and b null,
output is the list of distances for all points to all others ;
 if a or b is a number, consider b or a as list of numbers (1 dim).
If a and b are lists with not same size (i.e points with not same dimensions),
 returns NIL (no distance)."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::dist-euclid a b (if key key #'identity)))

(define-box dist-citybloc ((a nil) (b nil))
 "City-bloc distance between <x> and <y> which represent 'points' into kind of euclidian space.
If <y> is null, <x> may represent a list of points, then the output is the list of city-bloc distances for all points to all others."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::city-bloc a b))

(define-box dist-hamming ((a nil) (b nil) &optional (norm t) (test eql))
  "Hamming distance between list a and list b."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::hamming-dist a b norm test))

(define-box dist-edit ((seq1 nil) (seq2 nil) &key (sub 1) (ins 1) (del 1) (uncom 0) (norm NIL) (test equalp))
  :documentation "Editing distance (Levenstein distance) from sequence <seq1> to <seq2>.
Optional arguments are the following keywords :
<sub> : cost when substituting a symbol (1 by default) ;
<ins> : cost when inserting a symbol (1 by default) ;
<del> : cost when deleting a symbol (1 by default) ;
<uncom> : over-cost when inserting a symbol which is not common to both sequences (experimental, 0 by default) ;
<norm> : if nil, absolute values of distance, if T distance value is relative to the length of the shorter sequence ;
<test> : test function to compare symbols, may returns T if equal, NIL if not."
  :non-generic t
  :class morphologie-box
  ;:menu (norm (nil "not-normalized") (t "normalized"))
  (fv-morphologie::edit-dist seq1 seq2 sub ins del uncom norm test))

(define-box dist-multi-edit ((seq1 nil) (seq2 nil) (wgth 1) &key (change 1) (ins 1) (del 1) (uncom 0) (test equalp))
  :documentation "Multi-dimensional editing distance (experimental) : while editing distance compares symbols, multi-dimensional editiong distance compares lists of symbols where a list may be considered as a multi-dimensional symbolic description of some object (for instance '(A x 0) may describe some objet according to three symbolic dimensions where A, x and 0 are different symbolic values for each dimension.
Argument <wgth> is the weigths for each dimension. If a number, all dimensions have equal weigth, if a list, list of weigth given to each dimension.
All other keyword arguments are same as dist-edit."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::multi-edit-dist seq1 seq2 wgth :change change :insert ins :delete del :inex uncom :test test))

(define-box dist-structure  ((seq1 nil) (seq2 nil) &key (w-occ 1.) (w-rep 1.) (test equalp))
  :documentation "Distance of the structures of sequences seq1 and seq (experimental). Compare sequences according to their intrinsic redondancies or self-correlation."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::struct-dist seq1 seq2 w-occ w-rep test))

(define-box dist-graph ((v1 nil) (v2 nil) (graph nil))
  :non-generic t
  :class morphologie-box
  (fv-morphologie::tree-dist v1 v2 graph))


;;  3.3 QUANTIFIER : self information
(menu-separator :in EVALUATION)

(define-box histogram ((data nil) &key (test eql) (thes nil))
  :documentation "Histogram of a data set <data>. Output is a the list of lists (datum nb-of-occurancies)."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::histogramme data :test test :thes thes))

(define-box entropy ((data nil) &optional (mode nil) (samples nil) (test equalp))
  :documentation "Entropy evaluation of a data set <data>. Different modes are (optional argument) :
- shannon-2 : output value is relative to bits (log base 2) 
- shannon-n : output value is relative to the log based on the number of classes of symbols."
  :non-generic t
  :class morphologie-box
  :menu (mode (0 "shannon-2") (1 "shannon-n") (2 "cond-sh"))
  (when (not test) (setf test #'equalp))
  (cond ((not mode)
	 (message "Entropy: using 'shannon-2 mode (default).")
	 (fv-morphologie::shannon-entropy data 2 samples test))
	((zerop mode)
         (fv-morphologie::shannon-entropy data 2 samples test))
        ((= mode 1)
	 (fv-morphologie::shannon-entropy data nil samples test))
        ((= mode 2)
	 (fv-morphologie::entropie-conditionnelle data nil samples test))
	(t (message "Entropy: unknown mode. Available modes are: 'shannon-2, 'shannon-n, shannon-e."))))

(define-box elt-info ((data nil) (elt nil) &optional (test equalp))
  :non-generic t
  :class morphologie-box
  (fv-morphologie::self-info data elt test))

;(define-box redundancy ((data nil))
;  :non-generic t
;  :class morphologie-box
;  (fv-morphologie::redondance data))

(define-box  inner-dynamic ((seq nil) &optional (test equalp))
  :non-generic t
  :class morphologie-box
  (fv-morphologie::mark-dynamic seq test))

(define-box graph-length ((graph nil))
  :documentation "Returns the length of a graph (the length of the shorter path)."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::tree-minlen graph))

(define-box graph-degree ((node nil) (graph nil))
  :documentation "Returns the degree of a node in a graph."
  :non-generic t
  :class morphologie-box
  (fv-morphologie::tree-deg node graph))


;;;;;;;;;;;;;;;;
;;; 4. READ/WRITE files
(define-menu READ-WRITE :in fv-morphologie)
(in-menu READ-WRITE)

;;   read

(define-box read-text ((file nil)  (mode t) &key (sep nil) (rem-test nil))
  :non-generic t
  :class morphologie-box
  :menu (mode (t "lines") (nil "flat") (:ascii-7b "ascii-7b") (:ascii-8b "ascii-8b"))
  (case mode
    (:ascii-7b (read-txt-file file :mode :charcode :marks sep :rem-test rem-test :8bits nil))
    (:ascii-8b (read-txt-file file :mode :charcode :marks sep :rem-test rem-test :8bits t))
    (t (read-txt-file file :mode mode :marks sep :rem-test rem-test :8bits nil))))

; (define-box read-midi ((file))
; (define-box read-sdif ((file))

;;   write
(menu-separator :in READ-WRITE)

(define-box display-list ((list list))
  "Prints out to the Lisp listener each element of list within a newline with indentation relative to list level."
  :non-generic t
  :class morphologie-box
  (list-display list))

(define-box write-list ((list list) (file nil) &optional (mode :lisp))
  :non-generic t
  :class morphologie-box
  :menu (mode (:lisp "lisp") (:flat "flat") (:coll "Max coll"))
  (case mode
    (:coll (coll-write list file))
    (t (list-write list file mode))))

;;;;;;;;;;;;;;;;
; installing menu
(install-menu fv-morphologie)

#|
(defpackage :fv-morphologie-pw
  #+SBCL (:use :cl)
  #+Lispworks  (:use :cl :ompw)
  )

;;
(in-package :fv-morhologie-pw)
|#

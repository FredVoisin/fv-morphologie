;;;; common-lisp fv-morphologie main functions (menu)
;;;; copyright (c) Frederic Voisin, 1992-2012
;;;; Morphologie tool suite (Library) for Common-Lisp, OpenMusic (Ircam, 1997) and PWGL (Sibelius Academy, 2007).
;;;; First imagined and developped with Herve Riviere at LACITO-CNRS (1992) using Patchwork (Ircam 1991), most of these functions were originaly published with Jacopo Baboni-Schilingi (Ircam, 1997) - most of them are actually largely modified following some new experiences in music analysis, composition and teaching (cf. Voisin 2015, Vincenot). 
;;;; Common-Lisp
;;;; UTF-8

;; if no ASDF:

(load "~/projets/fv-morphologie/package.lisp")
(load "~/projets/fv-morphologie/fv-morphologie-encodage.lisp")
(load "~/projets/fv-morphologie/fv-morphologie.lisp")
(load "~/projets/fv-morphologie/fv-morphologie-graphs.lisp")
(load "~/projets/fv-morphologie/fv-morphologie-io.lisp")
;(load "~/projets/fv-morphologie/fv-morphologie-cl.lisp")

(in-package :fv-morphologie)

;;; FV-MORPHOLOGIE COMMON LISP MENU

;;;;;;;;;;;;
;;; 1. ENCODAGE
;;; 1.1 TO TRANSCODE

(defgeneric exsample (seq method mode thres &optional option)
  (:documentation "EXSAMPLE: samples a sequence of numerical values according to its signature.
Available methods are (see int-signature for documentation):
- :minmax
- :minflexmax (default)
- :landmarks
- :major-extrema
Modes may be :values (default), :positions :all or :resampled"))

(defmethod exsample ((seq list) (method null) (mode null) (thres number) &optional (option nil))
  (exsample seq :minflexmax :values thres option))

(defmethod exsample ((seq list) (method symbol) (mode symbol) (thres number) &optional (option nil))
  (if seq (echantillonage seq method mode thres option) nil))

(defgeneric split (seq &optional marks)
  (:documentation "SPLIT: splits <seq> according to marks. Seq may be a list or a string, marks must be a string defining a set of marks."))

(defmethod split ((seq list) &optional (marks " "))
  (split-list-using-marks seq nil))

(defmethod split ((seq string) &optional (marks " "))
  (split-string seq marks))

(defgeneric concaten (seq)
  (:documentation "CONCATEN: concatenates a sequence of symbols as a single symbol."))

(defmethod concaten ((seq t))
  (fv-morphologie::concat seq))

(defgeneric transcode (seq table &optional test)
  (:documentation "TRANSCODE: replaces each element of <seq> using a list of changes <table> with the following structure: '((old new) ...)."))

(defmethod transcode ((seq t) (table t) &optional (test #'eq))
  (fv-morphologie::transcode seq table test))

(defgeneric num>base (num base)
  (:documentation "NUM>BASE: converts an number to its representation in base <base>."))

(defmethod num>base ((num number) (base t))
  (fv-morphologie::num-base num base))

(defmethod num>alpha ((num number))
  (fv-morphologie::num>alpha num))

(defmethod alpha>num ((alpha t) &optional (mode nil))
  (fv-morphologie::alpha>num alpha))

(defmethod list>sym ((list t))
  (fv-morphologie::concaten list))

(defmethod graph>matrix ((list t))
  (fv-morphologie::graph2matrix list))

;;; 1.2 PREPROCESSING

(defgeneric filt-fct (seq func wind &optional step)
  (:documentation "Apply function <func> to sequence <seq> thru a sliding window of length <wind> by (optional) step <step>. Window size and step size may be an integer (samples) or float (seq/window ratio)."))

(defmethod filt-fct ((seq list) (func function) (wind number) &optional step)
  (cond ((zerop wind)
	 (window-apply seq func 2 step))
	 ((integerp wind)
	  (window-apply seq func (max 2 wind) step))
	 ((floatp wind)
	  (assert (<= wind .5))
	  (filt-fct seq func (round (* (length seq) wind)) step))
	 (t seq)))

(defgeneric filt-mean (seq wind &optional step)
  (:documentation "Values replaced by their respective  windowed mean values."))

(defmethod filt-mean ((seq list) (wind integer) &optional (step 1))
    (window-apply seq #'mean wind step))

(defmethod filt-mean ((seq list) (wind float) &optional (step 1))
    (window-apply seq #'mean (round (* wind (length seq))) step))

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
  "FILT-LOCAL-REP: remove identical values or symbols."
  (cond ((eq mode :delete)
         (fv-morphologie::rem-local-rep seq test))
        ((eq mode :linear)
         (if (not (member 'nil (mapcar #'numberp seq)))
             (fv-morphologie::nocons= seq test)
           (fv-morphologie::filt-local-rep seq :delete test)))
        (t (fv-morphologie::filt-local-rep seq :delete test))))

(defgeneric filt-noise (seq &optional mode val)
  (:documentation  "FILT-NOISE: jitters or adds some noise into sequence <seq> using different methods, according to the first optional argument <mode>:
 :nozero : replaces each zero value of <seq> with some random value
 :swap : substitue some value of seq by another
 :relative : "))

(defmethod filt-noise ((seq t) &optional (mode :nozero) (val 0.1))
  (fv-morphologie::fnoise seq mode val))

;;;;;;;;;;;;
;;; 2. DIFFERENTIATION
;;; 2.1 WITH INTENSITIES

(defgeneric int-signature (seq mode thres &optional option)
  (:documentation "INT-SIGNATURE: signature of a sequence with some threshold <thres> for variation in intensity.
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
with :landmarks method, <option> is the time (rank) distance threshold for detecting a major extrema."))

(defmethod int-signature ((seq t) (mode t) (thres number) &optional (option nil))
  (when (not mode) (setf mode :minflexmax))
  (signature seq mode thres option))

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
Use output menu <out> to define result form:
:struct lists all possible structures according to different marks of <seq>
:pos each structure is consed to the list of positions for each segment followed by the list of the segments themselves
:raw each structure is consed to the list of each segment with its positions in <seq>.
Option :diss defines the dissemblance threshold to consider different segment as the same.
Option :test defines the test function to consider the marks."))

(defmethod mark-structure  ((seq t) (out t) &key (diss 0) (rem-loc-dup t) (test #'equalp))
  (when not out (setf out :struct))
  (mark-strct seq out diss rem-loc-dup test))

;;; 2.3 USING MOTIFS

(defgeneric motif-find (motif seq &key diss l-var change ins del uncom test)
  (:documentation "MOTIF-FIND: returns all positions of a <motif> into a sequence <seq>.
Argument :diss is the threshold (from 0 to 1.0) of dissimilarity according to the normalized editing distance under which two different segments are considered to be the same ;
argument :l-var is the threshold for variation in length of the motifs to be compared ;
arguments :change :ins/sup :uncom and :test for tuning the editing distance (see dist-edit)."))

(defmethod motif-find ((motif t) (seq t) &key (diss 0) (l-var 0) (change 1) (ins 1) (del 1) (uncom 0) (test #'(lambda (a b) (dist-edit a b :norm T))))
  (find-pos motif seq diss l-var change ins del uncom test))

(defgeneric motif-list (seq out &key diss l-var n change insert delete uncom test)
  (:documentation "MOTIF-LIST: returns the list of all motifs found into <seq> where a motif is any segment repeted at least one time according to editing distance.
Output can be sorted according to the length of segment (:length) or to their frequency (:freq).
Argument :diss is a threshold (from 0 to 1.0) of dissimilarity according to the normalized editing distance under which two different segments are considered to be the same ;
argument :l-var is the threshold for variation in length of the segments to be compared ;
argument :n is the maxinum length of segments to be compared ;
arguments :change :ins/sup :uncom and :test for tuning the editing distance (see dist-edit)."))

(defmethod motif-list ((seq t) (out t)
		       &key (diss 0) (l-var 0) (n nil)
		       (change 1) (insert 1) (delete 1) (uncom 0) (test #'equalp))
  (when (not out) (set out :length))
  (find-self seq out diss l-var n change insert delete uncom test))

(defgeneric motif-structure (seq)
  (:documentation "MOTIF-STRUCTURE: to be set"))

(defmethod  motif-structure ((seq t))
  (fv-morphologie::rep-strct seq))

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
  (setf classes (min 2 classes))
  (case mode
    (nil (fv-morphologie::n-class data classes iter dist))
    (:centroids (fv-morphologie::n-class data classes iter dist))
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
  (setf classes (min 2 classes))
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
  (:documentation "DIST-MULTI-EDIT: "))

(defmethod dist-multi-edit ((seq1 t) (seq2 t) (wgth number) &key (sub 1) (ins 1) (del 1) (uncom 0) (test #'equalp))
  (fv-morphologie::multi-edit-dist seq1 seq2 wgth sub ins del uncom test))

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
  (:documentation "HISTOGRAM: histogram of data. With optional arguments:
test: identity test
thes: thesaurus or list of symbols to be considered."))

(defmethod histogram ((data t) &key (test #'eq) (thes nil))
  (fv-morphologie::histogramme data :test test :thes thes))

(defgeneric entropy (data &optional mode samples test)
  (:documentation "ENTROPY:"))

(defmethod entropy ((data t) &optional (mode nil) (samples nil) (test #'equalp))
  (cond ((not mode)
	 (message "Entropy: using 'shannon-2 mode (default).")
	 (shannon-entropy data 2 samples test))
	((eq mode 'shannon-2)
	 (shannon-entropy data 2 samples test))
	((eq mode 'shannon-e)
	 (shannon-entropy data (exp 1) samples test))
	((eq mode 'shannon-n)
	 (shannon-entropy data nil samples test))
	((eq mode 'cond-sh)
	 (entropie-conditionnelle data nil samples test))
    (t (message "Entropy: unknown mode. Avalaible modes are: 'shannon-2, 'shannon-n, shannon-e."))))

(defgeneric elt-info (data elt &optional test)
  (:documentation "ELT-INFO:"))

(defmethod elt-info ((data t) (elt t) &optional (test #'equalp))
  (fv-morphologie::self-info data elt test))

;(defmethod redundancy ((data t))
;  :non-generic t
;  :class morphologie-box
;  (fv-morphologie::redondance data))

(defgeneric inner-dynamic (seq &optional test)
  (:documentation ""))

(defmethod  inner-dynamic ((seq t) &optional (test #'equalp))
  (fv-morphologie::mark-dynamic seq test))

;;;;;;;;;;;;
;;; 4. DELINEATION
;;; 4.1 BUILDING GRAPHS OR TREES

(defgeneric graph-span (mat-dist &optional verbose)
  (:documentation   "GRAPH-SPAN: minimum spanning tree build on a semi-matrix of distances <mat-dist>."))

(defmethod graph-span ((mat-dist list) &optional (verbose nil))
  (minimum-spanning-tree mat-dist verbose))

(defgeneric graph-part (graph &optional mode)
  (:documentation  "GRAPH-PART: partition a graph according according to edges which have maximum length."))

(defmethod graph-part ((graph list) &optional (mode :distance))
  (part-graph graph mode))

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
  (:documentation "GRAPH-DEGREE: degree of a node in a graph."))

(defmethod graph-degree ((node t) (graph list))
  (fv-morphologie::tree-deg node graph))

(defgeneric graph-nodes (graph)
  (:documentation "GRAPH-NODES: list of all nodes of a graph."))

(defmethod graph-nodes ((graph list))
  (fv-morphologie::tree-nodes graph))

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
  (fv-morphologie::graph2dot graph distorsion scale shape out legend))

;;;;;;;;;;;;
;;; 5. IMPORT/EXPORT
;;; 5.1 READING FILES

(defgeneric read-text (file &key mode sep rem-test)
  (:documentation "READ-TEXT: reads text <file> using following <mode> formats:
 T : each line of file as a list
 NIL : flat mode, reads file as a single list
 :ascii-7b : reads file as a 7 bits ascii format
 :ascii- 8b : reads file as extended ascii (8 bits) format.
Additional keyword :sep is a string defining all characters defining a separator;
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
:lisp : list is written as a list defined as a variable (by default)
:flat : each element of list is written in a new line
:coll : each element of list is written in a new line in Max coll text file."))

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


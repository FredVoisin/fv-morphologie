;; ftools.lisp
;; Fred Voisin 2003-2007, v. 030907
;; outils divers pour fichiers, sequences, etc.
;; common-lisp

;;; Date & Time

(defun time-string ()
  (multiple-value-bind (s m h) (get-decoded-time)
    (format nil "~A:~2,,,'0@A:~2,,,'0@A" h m s)))

(defconstant months
  '#("Jan" "Feb" "Mar" "Apr" "May" "June" "Jul" "Aug" "Sept" "Oct" "Nov" "Dec"))

(defun date-string ()
  (multiple-value-bind (ig no re d mo y) (get-decoded-time)
    (declare (ignore ig no re))
    (format nil "~A ~A ~A" d (svref months (1- mo)) y)))

(defun date+time-string (&optional (u (get-universal-time)))
  (multiple-value-bind (s m h d mo y) (decode-universal-time u)
    (format nil "~A ~A ~A ~A:~2,,,'0@A:~2,,,'0@A"
                d (svref months (1- mo)) y h m s)))

;;; LISTES

(defun fflat (liste)
  (cond
   ((null liste) nil)
   ((atom liste) (list liste))
   (t (append (fflat (car liste))
              (fflat (cdr liste))))))

(defun flat (liste)
  (fflat liste))

(defun agree (x y)
  (if (or (null x) (null y))
      t
      (if (equal (car x) (car y))
          (agree (cdr x) (cdr y)))))

(defun assocify (source)
  (labels ((rec (source acc)
             (let ((rest (cddr source)))
               (if (consp rest)
                   (rec rest (cons (cons (car source) (cadr source)) acc))
                   (nreverse (cons (cons (car source) (cadr source)) acc))))))
    (if source (rec source nil) nil)))

(defun rotlist (x)      
  (if (cdr x) 
      (cons (car (last x)) (butlast x))
      x))


;;; FICHIERS 

(defun read-file-lines (file)
  (with-open-file (in-stream file
			     :direction :input
                             :element-type 'character)
    (loop with length = (file-length in-stream)
          while (< (file-position in-stream) length)
          collect (read-line in-stream))    ))

;(read-file-lines #P"files-utils.lisp")

(defun file-list (dir &optional extension)
  "attention: extension en  minuscules !"
  (when (not (equalp "/" (elt dir (1- (length dir)))))
    (setf dir (concatenate 'string dir "/")))
  (let ((d (directory (if extension
			  (concatenate 'string (directory-namestring dir) "*."
				       (if (stringp extension) extension (format nil "~(~@S~)" extension)))
			  (concatenate 'string (directory-namestring dir) "*.*")))))
    (mapcar #'namestring d)))

; (print  (file-list "/home/fred/lisp/midi/"))
; (print (file-list "/home/fred/lisp/midi" 'lisp))

(defun file-list-rec (paths &optional files)
  (if (null paths)
      files
    (multiple-value-bind (newfiles newpaths)
	(file-list (pop paths))
      (file-list-rec
       (append newpaths paths)
       (append newfiles files) ))))

; (print (file-list-rec '("/home/fred/lisp/")))

(defconstant whitechars '#(#\Space #\Tab #\Return #\Newline))


;;; SYMBOLS

(defun make-new-symbol (name &optional content)
    (let ((sym (intern (string (if (numberp (read-from-string (string name)))
                                                (gensym (format nil "NUMBER~S" name))
                                                (if (boundp (read-from-string (string name)))
                                                   (gensym (format nil "~S-" name))
                                                   name))))))
       (setf (symbol-value sym) content)
       sym ))

; (MAKE-NEW-SYMBOL "aaa")

(defun characters (string &optional code)
  (let ((c '()))
    (dotimes (n (length string) (reverse c))
      (if code
        (push (char-code (char string n)) c)
        (push (char string n) c)))))

;(characters "a b")

(defmethod repetition ((seq list) &key (test #'equalp) (preserve-whitespace 'nil))
  (declare (ignore preserve-whitespace))
  (remove 'nil
          (remove-duplicates
           (loop until (null seq) 
                 collect
                 (car (member (pop seq) seq)))
           :test test)))
 
(defmethod repetition ((seq string) &key (test #'equalp) (preserve-whitespace 'nil))
  (repetition (if preserve-whitespace
                 (characters seq)
                 (remove '#\Space (characters seq)) )
               :test test))
 
;(repetition '(1 2 3 2 1))
;(repetition "abac")
;(remove-if-not #'repetition '("a" "a b c a" "a b"))

(defun prefix (pref str)
  (search pref str :end2 (min (length pref) (length str))))

(defun suffix (str suff)
  (search suff str :start2 (- (length str) (length suff))))

(defun concatstrings (&rest lofstrings) 
  "Concantenates list of strings into one string."
  (let ((concatenated (make-string (apply #'+ (mapcar #'length lofstrings)) :initial-element #\Space))
        (n 0))
    (dolist (k lofstrings concatenated)
      (dotimes (i (length k))
        (setf (elt concatenated n) (elt k i))
        (setf n (1+ n))))))

(defmethod ascii2string ((ascii integer))
  (princ-to-string (code-char ascii)))

(defmethod ascii2string ((ascii list))
  (concatstrings (mapcar #'ascii2string ascii)))

(defun code2string (code)
  (ascii2string code))

;(code2string '(100 101 102 103))

(defun string2code (string)
  (let ((c '()))
    (dotimes (n (length string) (reverse c))
      (push (char-code (char string n)) c))))

(defmethod findpos ((item t) (seq t) &key (test #'=))
  (let (pos)
    (dotimes (i (length seq) (reverse pos))
      (when (funcall test (nth i seq) item)
        (push i pos)))))

(defmethod findpos ((item list) (seq t) &key (test #'=))
  (let (pos)
    (dotimes (i (length seq) (reverse pos))
      (when (member (nth i seq) item :test test)
        (push i pos)))))

(defmethod path2words ((path string))
  (let* (names (separators (findpos 58 (string2code path))))
    (dotimes (i (length separators))
      (push (subseq path
                    (1+ (nth i separators))
                    (nth (1+ i) separators))
            names))
    names))

(defmethod path2words ((path pathname))
  (setf path (princ-to-string path))
  (path2words path))

(defmethod segment-words ((seq string) &optional (marks " -/\"\_:,;"))
  ;; ajouter tabulation : (code2string 9)...
  (let* ((pos (findpos (append (string2code marks) (list 9)) (string2code seq)))
        words)
    (setf words (list (subseq seq 0 (car pos))))
    (dotimes (i (length pos))
      (push (subseq seq
                    (1+ (nth i pos))
                    (nth (1+ i) pos))
            words))
    (reverse (remove-if #'(lambda (x) (< (length x) 1)) words))))

(defun rename-string (string)
  (setf string (string2code string))
  (nsubstitute 33 32 string)
  (nsubstitute 43 42 string)
  (setf string (ascii2string string)))

(defmethod ndigit ((num integer))
  (cond ((< num 0) (1+ (floor (log (abs num) 10))))
        ((= 0 num) 1)
        (t (1+ (floor (log num 10))))))

(defmethod ndigit ((num symbol))
  (length (string num)))

(defmethod ndigit ((num string))
  (length num))

(defmethod ndigit ((num t))
  nil)

(defun list2string (list)
  (if (not (member 'nil (mapcar #'listp list)))
    (mapcar #'list2string list)
    (let ((string (make-string (1- (+ (apply #'+ (mapcar #'ndigit list))
                                      (length list)))
                               :initial-element #\Space))
          (i 0))
      (dotimes (n (length list) string)
        (let ((temp-string (format nil "~S" (nth n list))))
          (dotimes (k (length temp-string))
            (setf (elt string (+ i k))  (elt temp-string k)))
          (setf i (+ i (length temp-string) 1)))))))

;;--------------------------------
;; SDIF (cf. sdif.lisp)

(defun sdif-header-read (file)
  (let (temp header pos)
    (with-open-file (in file
			:direction :input
			:element-type 'character)
      (if (not (equalp "SDIF" (read-line in)))
	  (format t "ce fichier ne semble pas etre un fichier SDIF au format texte : arret.")
	  (loop until (equalp temp "SDFC")
	     do
	       (setf temp (read-line in))
	       (push temp header)))
      (setf pos (file-position in)))
    (values (reverse header) pos)))

;; (sdif-header-read "/home/fred/robin/poiscaille/poisson_elephant-fft.sdif.txt")

(defun sdif-type (header)
  (subseq header (position "1TYP" header :test #'equalp)))

;; (sdif-type head)

(defun sdif-mat-read (file &key start end (n 1))
  (let (line temp data (i 0) pos)
    (with-open-file (in file 
			:direction :input
			:element-type 'character)
      (when start (file-position in start))
      (loop with length = (if end end (file-length in))
	 while (and (< (file-position in) length) (< i n))
	 do
	   (setf line (read-line in))
	   (if (zerop (length line))
	       (progn (incf i)
		      (when (not (null temp))
			(setf data (append data (list (reverse temp))))
			(setf temp nil) ))
	       (push (mapcar #'read-from-string (segment-words line (code2string 9))) temp)))
      (setf pos (file-position in)))
    (values data pos)))

;; (sdif-mat-read "/home/fred/robin/poiscaille/poisson_elephant-fft.sdif.txt" :start 649 :n 1)

(defun sdif-fftframe-read (file &key start end (n 1) (verbose nil))
  ;; lit les frames fft avec la date, le format etant presume
  (let (data (i 0))
    (with-open-file (in file 
			:direction :input
			:element-type 'character)
      (when start (file-position in start))
      (loop with length = (if end end (file-length in))
	 while (and (< (file-position in) length) (< i n))
	 do
	   (let ((line (read-line in))  time tmp k)
	     (when (zerop (length line))
	       (incf i)
	       (setf line (read-line in)))
	     (when verbose (format t " ~S/~S" i n))
	     (setf time (read-from-string (fourth (segment-words line (code2string 9)))))
	     (read-line in) ;; pour allaer plus vite on ignore 
	     (read-line in)
	     (setf k (read-from-string (third  (segment-words (read-line in) (code2string 9)))))
	     (dotimes (j k)
	       (push (read-from-string (read-line in)) tmp))
	     (push (list time (reverse tmp)) data))))
    (nreverse data)))

;; (print (sdif-fftframe-read "/home/fred/robin/poiscaille/poisson_elephant-fft.sdif.txt" :start 702 :n 2))

(defun read-sdif-fft (file &key (end nil) (n nil) (verbose nil))
  ;; start frame pas encore implemente...
  (let (fftinfo nframes pos)
    (multiple-value-bind (h p) (sdif-header-read file)
      (declare (ignore h))
      (setf ;header h
	    pos p))
    (multiple-value-bind (info p) (sdif-mat-read file :start (+ pos 1) :n 1)
      (setf fftinfo info
	    pos p))
    (setf nframes (car (last (car (last (car fftinfo))))))
    (format t "~&nombre frames = ~S" nframes)
    (sdif-fftframe-read file :start pos :end end :n n :verbose verbose)))

;;(time (setf f0-10000 (read-sdif-fft "/home/fred/robin/poiscaille/poisson_elephant-fft.sdif.txt" :n 9999)))


;;----------------------------------------------------------------
;; SEQUENCES

(defun seq-member (s1 s2)
  (let ((a nil) (l1 (length s1)) (l2 (length s2)))
    (loop for s from 0 to (1- l2)
          until (setf a (equalp s1
                                (subseq s2 s
                                        (funcall #'min l2 (+ s l1)))))
          finally (return a))))

;(seq-member '(1 2 3) '(4 3 3 2 3 5 6 7 1 2 3 6 7))

(defgeneric distance-edit (seq1 seq2 &key replace insert mode exo)
  ; distance d'edition, 04 2007 ;-)
  )

(defmethod distance-edit ((seq1 list) (seq2 list)
			  &key (replace 1) (insert 1) (mode 0) (exo 0) )
  (let ((matcouts()) d d1 d2 d3 c c1 (change replace) (ins/sup insert) ex ey)
    (dotimes (j (+ (length seq2) 1))
      (dotimes (i (+ (length seq1) 1))
        (setf d (+ i (* j (+ (length seq1) 1))))
        ;;;--- SI i et j differents de 0 ---
        (cond ((and (> i 0) (> j 0))
               ;;test si ai et bj elements (respectivement) des chaines n et m
               (if (eq (member (nth (- i 1) seq1) seq2) nil)
		   (setf ex 1) (setf ex 0))
               (if (eq (member (nth (- j 1) seq2) seq1) nil)
		   (setf ey 1) (setf ey 0))
               ;;calcul du cout
               (if (eq (nth (- i 1) seq1) (nth (- j 1) seq2))
		   (setf c1 0)
		   (setf c1 (+ change (* (+ ex ey) exo))))
               ;;calcul de D(i,j)
               (setf d1 (nth (+ 1 (length seq1)) matcouts)
		     d2 (nth (length seq1) matcouts)
		     d3 (nth 0 matcouts))
               ;;calcul de cout mini
               (setf c (apply 'min
                        (list
                         (+ c1 d1) (+ ins/sup (* ey exo) d2) (+ ins/sup (* ex exo) d3)
                         )))
               (push c matcouts))
              ;;;--- SI i ou j = 0 ---
              (t
               ;;calcul du cout
               (if (and (eq i 0) (eq j 0))
		   (setf c1 0)
		   (setf c1 ins/sup))
               ;;si i = 0 et j> 0
               (cond ((and (eq i 0) (> j 0))
                      ;;test si bj element de seq1
                      (if (eq (member (nth (- j 1) seq2) seq1) nil)
			  (setf ey 1) (setf ey 0))
		      ;;calcul de D(i,j)
                      (setf d (nth (length seq1) matcouts))
                      (setf c (+ c1 d (* ey exo)))
                      (push c matcouts))
                     ;;si i > 0 et j = 0
                     ((and (> i 0) (eq j 0))
                      ;;test si ai element de seq2
                      (if (eq (member (nth (- i 1) seq1) seq2) nil)
			  (setf ex 1) (setf ex 0))
		      ;;calcul de D(i,j)
                      (setf d (nth 0 matcouts)
			    c (+ c1 d (* ex exo)))
                      (push c matcouts))
                     (t (push '0 matcouts) (setf c '0))))) ))
    (case mode
      (2 (list (car matcouts) (float  (/ (car matcouts) (max (length seq1) (length seq2))))))
      (1 (float (/ (car matcouts) (max (length seq1) (length seq2)))))
      (0 (car matcouts)))))

(defmethod distance-edit ((seq1 t) (seq2 t)
			  &key (replace 1) (insert 1) 
			  (exo 0)  (mode 0))
  (distance-edit (string seq1) (string seq2) :replace replace :insert insert
                     :mode mode :exo exo))

(defmethod distance-edit ((seq1 string) (seq2 string)
			  &key (replace 1) (insert 1) 
			   (mode 0) (exo 0))
  (distance-edit (characters seq1) (characters seq2) :replace replace :insert insert
                     :mode mode :exo exo))

(defmethod ldistance-edit ((seqlist t) &key (replace 1) (insert 1) 
			   (mode 0) (exo 0))
  (let ((l (length seqlist)) r)
    (dotimes (i l (nreverse r))
      (let (temp)
	(dotimes (j (- l i 1) (when temp (push (nreverse temp) r)))
	  (push (distance-edit (nth i seqlist) (nth (+ i j 1) seqlist)
			       :replace replace :insert insert :mode mode :exo exo)
	      temp))))))

;(findpos 1 '(0 1 2 0 1 0 1 20 1 2 0 1 2 1 0 1))
;(findpos '(0 1 5) '(0 1 2 0 1 0 1 20 1 2 0 1 2 1 0 1))
;(ascii2string 118)
;(ascii2string '(118 119 101))
;(defvar teststring "../www")
;(string2code teststring)
;(path2words teststring)
;(segment-words "Cello batt" )
;(mapcar #'segment-words (path2words teststring))

;(distance-edit '(1 2 1 3 4) '(2 3 4))
;(distance-edit '(1 2 1 3 4) '(2 3 4) :mode 1)
;(distance-edit '(1 2 1 3 4) '(2 3 4) :mode 2)
;(distance-edit "abwca" "abdca" :exo 1)
;(distance-edit "home:fred:123" "home:fred:456" )
;(ldistance-edit '((1 2 3) (2 3 4 1) (1 3 3) (4 3 4 1) (1 2 3)))


;; CLASSES, HELP...

(defgeneric subclass (symbol class)
   (:documentation
   "Tests if <symbol> is a subclass of <class>.
<symbol> and <class> must be both symbols."))

(defmethod subclass ((symbol symbol) (class symbol))
   (let ((class-found (find-class symbol nil)))
     (if class-found
       (if (member class
                   (mapcar #'class-name 
                           (CLASS-PRECEDENCE-LIST (find-class symbol nil))))
         t nil)
       (format t (format nil
                         "Error : class ~S does not exist."
                         symbol)))))

(defmethod find-subclasses ((root symbol))
  "Give the list of sub-class of root."
  (let ((classes '()))
    (do-symbols (sym)
      (let ((new-class (find-class sym nil)))
        (when (and new-class
                   (subtypep new-class root))
          (push new-class classes))))
    (sort (remove (find-class root)
                  classes)
          #'string-lessp :key #'class-name)))

(defmethod find-subclasses ((root standard-class))
  "Give the list of sub-class of root."
  (find-subclasses (class-name root)))

(defmethod find-subclasses ((root null))
  "Give the list of sub-class of root."
  nil)

;(find-subclasses 'number)
;(find-subclasses nil)

#|
(CLASS-PRECEDENCE-LIST (find-class 'number nil))
|#

(defun find-classes (&optional (package 'cl-user))
  "Give the list of sub-class of root."
  (let ((classes '()))
    (do-symbols (sym)
      (let ((new-class (find-class sym nil)))
        (when (and (eql 'standard-class (type-of new-class))
                   (eq (find-package package)
                       (SYMBOL-PACKAGE (class-name new-class))))
          (push new-class classes))))
    (sort classes
          #'string-lessp :key #'class-name)))

;(find-classes)

(defun generic-function-p (self)
  "T if self is a class of <standard-generic-function>."
  (or (eq (class-name
           (class-of self))
          'standard-generic-function)
      (eq (class-name
       (class-of self))
      'compiled-function)))

(defun function-p (self)
  "T if self is a class of <standard-generic-function>."
  (or (eq (class-name
           (class-of self))
          'standard-generic-function)
      (eq (class-name
           (class-of self))
          'compiled-function)))

(defun type-p (self type)
  "T if self is a class of <standard-generic-function>."
  (if (member (find-class self nil)
              (find-subclasses type))
    t nil))

;(type-p 'bit 'number)

(defun find-functions (&optional (package (find-package 'cl-user)))
  "Give the list of generic functions in <package>."
  (when (symbolp package) (setf package (find-package package)))
  (let
    ((func '()))
    (do-symbols (sym package)
      (when (eq package (symbol-package sym))
        (let ((value (when (fboundp sym) (symbol-function sym))))
          (when (and value
		     (or (compiled-function-p value)
			 (function-p value)
			 (generic-function-p value)))
            (push sym func)))))
    (sort func #'string-lessp)))

; (find-functions)
; (length (find-functions))

(defun find-instances (class &optional (package (find-package 'cl-user)))
  "Give the list of insrtances of <class> in (optional) <package>."
  (when (symbolp package) (setf package (find-package package)))
  (let
    ((instances '()))
    (do-symbols (sym package)
      (when (eq package (symbol-package sym))
        (let ((value
               (when (boundp sym)
                 sym)))
          (when (and value
                     (eq (find-class class) (class-of (eval value))))
					;(generic-function-p value))
            (push sym instances)))))
    instances))

(defun cherche (name &optional (n 6) &key (package 'cl-user))
  (when (symbolp package) (setf package (find-package package)))
  (let ((r (find-functions package))
	(nom (when (stringp name) name (string name))))
    (do-symbols (sym package)
      (when (boundp sym)
	(push sym r)))
    (mapcar 'cadr
	    (subseq
	     (sort (loop for i from 0 to (1- (length r))
			 collect
			 (list (distance-edit  (string (nth i r)) nom
					       :insert 1 :replace 1 :exo 2)
			       (nth i r)))
		   '< :key 'first)
	     0 n))))

;; (distance-edit "stydfaSF" "styDBCLHDSCL" :INSERT 1 :replace 1 :exo 2 :test nil)

; (cherche "DISTANCE")
; (cherche "concat")
; (cherche "CONCAT")
; (cherche "FILE")
; (cherche "LIST")
; (cherche "LIST" 20)

;;; formats

;; list to array
(defun list2array (liste)
  (if (not (member nil (mapcar 'atom liste)))
      (let ((xx (make-array (length liste))))
	(dotimes (i (length liste))
	  (setf (aref xx i) (nth i liste)))
	xx)
      (print "not yet defined")))


;;; ----------------------------------------------------------------
;;; some MATHS & STATS

(defun histogramme (data &optional (test #'equalp))
  (let ((cl (remove-duplicates data))
	(histo (list)))
    (dolist (c cl)
      (push (list c (length (remove-if-not #'(lambda (x) (funcall test c x))
					   (copy-tree data))))
	    histo))
    (sort histo '< :key 'car)))

(defun average (&rest numbers)
  (float (/ (apply #'+ numbers) (length numbers))))

;(average 1 2 3 4 5 6)

(defun dx (serie)
  (let (r)
    (dotimes (i (- (length serie) 1) (nreverse r))
      (push (- (nth (1+ i) serie) (nth i serie)) r))))

(defun e-boltzmann (data)
  "S = k Ln O"
  (let ((k (* 1.381 (expt 10 23)))
	(O (length  (remove-duplicates data))))
    (* k (log O))))

#|
(e-boltzmann '(a s a c f q s a c f))
(e-boltzmann '(a s a c s a c s c a))
(e-boltzmann '(0 0 0 0 0 0 0 0 0 0))
(e-boltzmann '(0 0 0 0 1 0 0 0 0 0))
(e-boltzmann '(1 1 1 1 1 0 0 0 0 0))
(e-boltzmann '(1 0 1 0 1 0 1 0 1 0))
|#

(defun e-shannon (data &key (test #'equal) (base 2))
  "valeur d'entropie de Shannon de la liste data.
I = k log (1/Pn)
Cf. Henri Atlan : L'organisation biologique et la theorie de l'information, Hermann, Paris, 1972."
  (let* ((cl (remove-duplicates data))
	 (N (length data))
	 (P ()))
    (dolist (ci cl (* -1 (apply #'+ (mapcar (lambda (x) (* x (log x base))) P))))
      (push (/ (length (remove-if-not (lambda (x) (funcall test x ci)) data)) N)
	    P))))

#|
(e-shannon '(a s a c f q s a c f))
(e-shannon '(a s a c s a c s c a))
(e-shannon '(0 0 0 0 0 0 0 0 0 0))
(e-shannon '(0 0 0 0 1 0 0 0 0 0))
(e-shannon '(1 1 1 1 1 0 0 0 0 0))
(e-shannon '(1 0 1 0 1 0 1 0 1 0))
|#

(defun windowed-e-shannon (data n &key (overlap .5) (base 2))
  ;; applique e-shannon sur fenetres de taille n echantillons
  ;; avec overlapping = fraction de n.
  ;; a modifier avec nbre classe total constant pour chaque fenetre ?
  (let* ((L (length data))
	(step (floor (* n (- 1 overlap))))
	(r (list)))
    (loop for i from 0 to (1- (floor (/ L step)))
	 do
	 (push (e-shannon (subseq data
				  (* i step)
				  (if (< (+ n (* i step)) L)
				      (+ n (* i step))
				      L))
			  :base base)
	       r))
   (nreverse r)))

(defun dec2base (x &optional base)
  ; converti x exprime en decimal en base, ou en toute lettre sinon 
  (if base
      (read-from-string (format nil (concatenate 'string "\~" (format nil "~S" base) "r") x))
      (read-from-string (format nil "~r" x))))

;(dec2base 12 2)
;(dec2base 16 16)
;(dec2base 21)

(defun norandom (n &key (base 2) (noise 0) (min 2) (max nil))
  ;developper en specifiant (random z motifs possibles) + variations
  (when (not max) (setf max (floor (/ n (expt 2 (1+ (log n 10)))))))
  (assert (<= max (/ n 2)))
  (let* ((k (+ min (random max)))
	 (l (floor (/ n k)))
	 (rest (- n (* k l)))
	 seq res)
    (dotimes (i k) (push (random base) seq))
    (setf res (append (apply #'append (make-list l :initial-element seq))
		      (subseq seq 0 rest)) )
    (if (zerop noise)
	res
      (dotimes (z (length res) res)
	(when (< (random 1.0) noise)
	  (setf (nth z res) (mod (+ (nth z res) (random base)) base)))))))

#|
(norandom 10)
(norandom 10 :base 4)
(norandom 10 :base 3 :min 3)
(norandom 10 :noise .2)
(norandom 100)
(time (norandom 100 :base 8 :noise .1))
(time (norandom 1000000 :base 8 :noise .1))
|#

#|
(loop for k from 0 to 100
      collect
      (apply #'average
	     (windowed-e-shannon
	      (loop for i from 0 to 200 collect (random 2))
	      20)) )

(loop for k from 0 to 100
      collect
      (apply #'average
	     (windowed-e-shannon (print (norandom 200)) 20) ) )


(windowed-e-shannon (norandom 1000) 10)

|#


;;;---------------------------------------------------------------
;;; Listing des fonctions 
#|
(dolist (f (find-functions))
  (print f))
|#

;;; EOF

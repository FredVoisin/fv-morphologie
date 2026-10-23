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

;;;; utils.lisp
;;;;
;;;; General variables, maths, lists, matrices and other utilities.

(in-package :fv-morphologie)

(defun safe-read-from-string (string &rest args)
  "read-from-string with *read-eval* nil: the reader macro #. in string (for instance a word of a text file)
signals an error instead of evaluating its code."
  (let ((*read-eval* nil))
    (apply #'read-from-string string args)))

;;; GENERAL VARIABLES

(defparameter *alpha* (quote (#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #.(code-char 176))))

(defparameter *num* (quote (#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))

(defparameter *months* '("january" "february" "march" "april" "may" "june" "july" "august" "september" "october" "november" "december"))

(defun date ()
  (multiple-value-bind (s m h d mo y a b c) (get-decoded-time)
    (declare (ignore a b c s))
    (list y mo d h m)))
    
;;; FOR WARNINGS ETC.

(defun message (message)
  (format t "~%~S~%" message))

;;; lisp hacks if necessary

(defun sum-loop (list)
"Apply #'+ may not work with huge lists in LW"
  (let ((r 0))
    (dolist (x list r)
      (incf r x))))

;;; SOME "MATHS"

(defun log! (x b)
  (if (zerop x) 0 (log x b)))

(defun mean (data)
  "The mean of the numbers of data (an error if data is empty)."
  (if data
      (float (/ (apply #'+ data) (length data)))
      (error "mean: no value (the list is empty).")))

(defun xor (a b)
  (if (and a b) nil (or a b)))

(defun test (mode)
  (case mode
    (null 0)
    (:charcode 2)
    (t 1)))

;;;; sept 2009, kind of deleuzian differentiation (sept. 2009)...

(defgeneric a-zero (x &optional quasi-zero)
 (:documentation "Replace all zeros by small value (optional: quasi-zero value).
If x is a graph or tree (a list of lists), change only the third element of each node considered to be a distance."))

(defmethod a-zero ((x number) &optional (quasi-zero .001))
  (if (zerop x) (random quasi-zero) x))

(defmethod a-zero ((x list) &optional (quasi-zero .001))  ;; not to be removed used for tres etc
  (if (not (member nil (mapcar #'listp x)))
      (mapcar #'(lambda (a) (append (butlast a) (a-zero (last a) quasi-zero))) x)
      (mapcar #'(lambda (a) (a-zero a quasi-zero)) x)))

(defun normalize (data)
  "The numbers of data divided by their sum, which is then 1 (an error if the sum is 0)."
  (let ((sum (apply #'+ data)))
    (cond ((null data) nil)
          ((zerop sum) (error "normalize: the sum of ~S is 0." data))
          ((= 1 sum) data)
          (t (mapcar #'(lambda (x) (/ x sum)) data)))))


;;; then, quasi-zero should me renamed schism and be a kind of random number when 0 is a float (epsilon = eteron ?), not integer
;;; ( 0 =? nil) 
;;; in a dynamical - chaotic - dynamic model for emergence 
  
(defun dnoise (x y &optional (test #'=))
  (let* ((s (- y x))
         (z (funcall test x y)))
    (if z
        (if (minusp s)
            (+ y (random (abs s)))
          (+ x (random s)))
      (values x y))))


;;; FOR COMPATIBILITIES (OM, PWGL etc)

(defun list! (self)
  (if (listp self) self (list self)))

(defun mat-transpose (liste)
  (assert (apply #'= (mapcar #'length liste)) nil
	  "we usually consider regular matrix. so, this should not happen. Please report this to Kilian Sprotte or Fred Voisin")
  (when liste (apply #'mapcar #'list liste)))

; when list have decreasing size, recursively (used by newold function)
(defun ldlrot (ldl &optional (r (list)))
  (if (caar ldl)
      (ldlrot (mapcar #'cdr ldl) (cons (remove 'nil (mapcar #'car ldl)) r))
      (reverse r)))

(defun group-list (liste segmentation mode)
  "Segments <liste> in successives sublists 
which lengths are successive values of the list <segmentation>.
 <mode> indicates if <liste> is to be read in a circular way."
  (let ((list2 liste) (res nil))
    (catch 'gl
      (loop for segment in segmentation
	 while (or list2 (eq mode 'circular))
	 do (push (loop for i from 1 to segment
		     when (null list2)
		     do (ecase mode
			  (linear (push sublist res) (throw 'gl 0))
			  (circular (setf list2 liste)))
		     end collect (pop list2) into sublist
		     finally (return sublist))
		  res)))
    (nreverse res)))

(defun deep-sort-list (liste &optional (pred '<)) ; ex sort-list
  (cond ((null liste) nil)
        ((atom (first liste)) (sort liste pred))
        (t (cons (deep-sort-list (first liste) pred) (deep-sort-list (rest liste) pred)))))

(defun flat-1 (liste)
  (if (consp (car liste))
      (apply 'append liste)
      liste))

(defun flat0 (tree res r)
  "At first in LispMe on my Visor :)"
  (cond ((null tree)
	 (if (null res) r (flat0 (car res) (if (null res) '() (cdr res)) r)))
	((listp tree) (flat0 (car tree) (cons (cdr tree) res) r))
	(t (progn
	     (setf r (cons tree r))
	     (flat0 (if (null res) '() (car res))
		    (if (null res) '() (cdr res))
			r)))))

(defun flat (list)
 (reverse (flat0 list '() '())))

;(flat '(YOU (WALRUS (HURT THE) ONE ((YOU) LOVE))))

(defun dx (seq &key key)
    (let (r (n-1 (pop seq)))
      (loop while seq
	 do (let ((n (pop seq)))
	      (if (not key)
		  (push (- n n-1) r)
		  (progn
		    (push (copy-tree n) r)
		    (eval `(setf (,key (car (quote ,r)))
			   (- (,key (quote ,n))
			      (,key (quote ,n-1)))))))
	      (setf n-1 n)))
      (nreverse r)))

; (dx '(1 2 3 3 5 6 5 3  8 9))
; (dx '((1 2) (3 3) (5 6) (5 3) (8 9)) :key 'cadr)

(defun derivee (seq)
  "The signs of the successive differences of the numbers of seq: 1 when the next value is greater,
-1 when it is smaller, 0 when it is equal (one element less than seq)."
  (loop for (a . rest) on seq
        while rest
        collect (let ((b (car rest)))
                  (cond ((> b a) 1)
                        ((< b a) -1)
                        (t 0)))))

; (derivee '(1 2 3 3 5 6 5 3 8 9)) => (1 1 0 1 1 -1 -1 1 1)


;;; ABOUT LISTS

(defun lmin (liste)
  "The minimum of the numbers of liste (an error if liste is empty)."
  (if liste (reduce #'min liste) (error "lmin: no value (the list is empty).")))

(defun lmax (liste)
  "The maximum of the numbers of liste (an error if liste is empty)."
  (if liste (reduce #'max liste) (error "lmax: no value (the list is empty).")))

(defgeneric findpos (item seq &key test)
  (:documentation "finds all positions of items into seq."))

(defmethod findpos ((item t) (seq t) &key (test #'eql))
  (let (pos)
    (dotimes (i (length seq) (nreverse pos))
      (when (funcall test (nth i seq) item)
        (push i pos)))))

(defmethod findpos ((item null) (seq t) &key (test #'eql))
  (declare (ignore test item))
  seq)

(defmethod findpos ((item list) (seq t) &key (test #'eql))
  (let (pos)
    (dotimes (i (length seq) (nreverse pos))
      (when (member (nth i seq) item :test test)
        (push i pos)))))

(defun split-list-using-marks (liste marks)
  "Splits liste at its marks, which are not kept: marks is a list of marks (compared with equalp),
a predicate, or a single mark. Empty segments are removed."
  (let ((markp (cond ((functionp marks) marks)
                     ((listp marks) #'(lambda (e) (member e marks :test #'equalp)))
                     (t #'(lambda (e) (equalp e marks)))))
        (r nil)
        (segment nil))
    (dolist (e liste)
      (if (funcall markp e)
          (when segment (push (nreverse segment) r) (setf segment nil))
          (push e segment)))
    (when segment (push (nreverse segment) r))
    (nreverse r)))
	
;(split-list-using-marks '(1 2 3 4 1 2 3 1 2 1) '(1 2))
;(split-list-using-marks '(1 2 3 4 1 2 3 1 2 1) #'(lambda (x) (< x 3)))

(defun concat (liste)
  (if (atom liste) liste
      (let ((a (car liste)))
	(cond ((listp a)
	       (apply #'append liste))
	      ((stringp a)
	       (eval `(concatenate 'string .,liste)))
	      (t
	       (safe-read-from-string (concat (mapcar #'(lambda (x) (format nil "~S" x))
						 liste))))))))

(defun 2first (liste)
  (if liste
      (subseq liste 0 2)
    nil))

(defun randnth (list)
  (nth (random (length list)) list))

(defun randswap (list &optional iter)
  (when (not iter) (setf iter (floor (/ (length list) 2))))
  (let ((l (copy-tree list))
        (le (length list)))
    (loop repeat iter
       do (let* ((i (random le))
		 (j (random le))
		 (k (nth i l)))
	    (setf  (nth i l) (nth j l)
		   (nth j l) k)))
    (values l)))

;(randswap '(1 2 3 4 5 6 7 8 9 0) 10)

(defgeneric randsort (list &optional n)
  (:documentation "Sort randomly elements of list.
Optional argument is the amount randomness if <n> a float (0.0 to 1.0) or the number of iterations if <n> is an integer
(where each iteration change the place of one element of list)."))

(defmethod randsort ((list list) &optional (n 1.0))
  (randswap list (if (floatp n) (floor (* n (length list))) n)))

(defun list-print (liste &optional (stream t))
  "Prints out list into listener."
  (dolist (l liste (format t "~&"))
    (format stream "~&~S" l)))

(defun list-display (tree &optional res (n 0))
  (cond ((null tree) (if res (list-display (car res) (cdr res) (1- n)) (values)))
	((listp tree) (list-display (car tree) (cons (cdr tree) res) (1+ n)))
	(t (progn (format t "~&")
		  (dotimes (i n (format t "~S" tree)) (format t " "))
		  (list-display (car res) (cdr res) (1- n))))))

;(list-display '(YOU (WALRUS (HURT THE) ONE ((YOU) LOVE))))

(defun list-save (liste file)
  "Writes list to file."
  (with-open-file (out-stream (if file file "liste")
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
    (format out-stream "(~&")
    (dolist (l liste (format out-stream "~&)"))
      (format out-stream "~&~S" l))))

(defun list-mod (liste &optional ncol) ; list-modulo box
  "partitions LISTE in NCOL lists containing the elements modulo NCOL"
  ;; new implementation by Kilian
  ;; should produce the same result
  ;; I assume that NCOL would not be a very
  ;; large number - so efficiency should be fine
  (let ((result-lists (make-list ncol)))
    (loop for i = 0 then (mod (1+ i) ncol)
       for elt in liste 
       do (push elt (nth i result-lists)))
    (map-into result-lists #'nreverse result-lists)))

(defun less-deep-mapcar (fun liste &rest args)
  "Applies FUN to LISTE ARGS if LISTE is a one-level list;
   Mapcars FUN to LISTE ARGS if LISTE is a multi-level list. "
  (cond ((null liste) nil)
	((atom (car liste)) (apply fun liste args))
	((atom (car (car liste)))
	 (cons (apply fun (car liste) args)
	       (apply #'less-deep-mapcar fun (cdr liste) args)))
	(t
	 (cons (apply #'less-deep-mapcar fun (car liste) args)
	       (apply #'less-deep-mapcar fun (cdr liste) args)))))

(defun deep-mapcar (fun fun1 liste &rest args)
  "Mapcars FUN or applies FUN1 to LISTE ARGS whether LISTE is a list or not."
  (cond ((null liste) nil)
	((not (consp liste)) (apply fun1 liste args))
	(t
	 (cons (apply #'deep-mapcar fun fun1 (car liste) args)
	       (apply #'deep-mapcar fun fun1 (cdr liste) args)))))

(defun sort-list-char (list &optional fct) ;; todo
  "Ascending sort of characters and numbers."
  (sort list '<	:key #'(lambda (x)
			 (if (numberp (if fct (funcall fct x) x))
			     (* 1000 (if fct (funcall fct x) x))
			     (char-code (char (string (if fct (funcall fct x) x)) 0))))))

;;;;;;;;;;;;;;;;
;;; SOME NUMERIC DATA PROCESSING

; fmax fmin because of limitations of min max in LW...
(defun fmax (seq)
  "max value of s (an error if seq is empty)"
  (if seq (reduce #'max seq) (error "fmax: no value (the list is empty).")))

(defun fmin (seq)
  "min value of s (an error if seq is empty)"
  (if seq (reduce #'min seq) (error "fmin: no value (the list is empty).")))

(defun permut-p (l1 l2)
  (not (or (member 'nil (mapcar #'(lambda (x) (member x l1)) l2))
	   (member 'nil (mapcar #'(lambda (x) (member x l2)) l1)))))


;;; MATRICES

(defun const*matrix (const matrix)
  (let ((m (nth 0 (array-dimensions matrix)))
	(n (nth 1 (array-dimensions matrix)))
	result)
    (setq result (make-array (list m n)))
    (dotimes (o m result)
      (dotimes (p n) (setf (aref result o p) (* const (aref matrix o p)))))))

(defun multiply-two-matrices (a-matrix b-matrix &key (result (make-array (list
									  (nth 0 (array-dimensions a-matrix))
									  (nth 1 (array-dimensions b-matrix))))))
  "Given
   [1] a-matrix (required)
       ==> a 2d matrix
   [2] b-matrix (required)
       ==> another 2d matrix, with dimensions such that the product of a-matrix and b-matrix is defined
   [3] result (keyword; new 2d array of appropriate size)
       <== a 2d matrix to contain product of two matrices
returns
   [1] product of two matrices (placed in result)"
  (let ((m (nth 0 (array-dimensions a-matrix)))
	(n (nth 1 (array-dimensions b-matrix)))
	(common (nth 0 (array-dimensions b-matrix))))
    (dotimes (i m result)
      (dotimes (j n)
	(setf (aref result i j) 0.0)
	(dotimes (k common)
	  (incf (aref result i j)
		(* (aref a-matrix i k) (aref b-matrix k j))))))))

(defun transpose (a-matrix &key
		  (result (make-array (reverse (array-dimensions a-matrix)))))
  "Given
   [1] A (required)
       ==> a 2d matrix
   [2] result (keyword; new 2d array of appropriate size)
       <== a 2d matrix to contain transpose of a-matrix
returns
   [1] transpose of a-matrix (placed in result)"
  (let ((list-of-two-integers (array-dimensions a-matrix)))
    (dotimes (i (nth 0 list-of-two-integers) result)
      (dotimes (j (nth 1 list-of-two-integers))
	(setf (aref result j i) (aref a-matrix i j))))))

(defun matvar (matrix)
  (const*matrix (/ 1 (car (array-dimensions matrix)))
		(multiply-two-matrices (transpose matrix) matrix)))

(defun -reverse (list r)
  "A special working reverse for dotted lists."
    (if (listp list)
	(-reverse (cdr list) (cons (car list) r))
      (cons list r)))

;(-reverse (cons 1 '(2)) nil) !!crash
;(-reverse (cons 1 2) nil)
;(-reverse '(1 2 . 3) nil)
; > flatten

;;; series
;; some tools for series

(defun xt2dt (xt)
  (let ((d (list (pop xt))))
    (dolist (x xt (reverse d))
      (when (not (= (cadr x) (cadar d)))
        (push x d)))))

;;; shell utils

#-Lispworks
(defun shell-command (command &rest args)
  "Runs COMMAND (a string or a list of program and arguments), see uiop:run-program for ARGS."
  (apply #'uiop:run-program command args))

#+Lispworks
(defun shell-command (string &rest args)
  (eval `(system:run-shell-command ,string .,args)))

;(shell-command "ls")

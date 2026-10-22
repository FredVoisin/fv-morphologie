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

;;;; fv-morphologie.lisp
;;;;
;;;; Lisp tools to analyse sequences of symbols or signs wich represent music,
;;;; for PWGL and OpenMusic (and Lisp).
;;;;
;;;; Frederic Voisin, Paris - Montbeliard, 1994-2010
;;;; rewriting of morphologie for Common-Lisp, OpenMusic and PWGL
;;;;
;;;; Thank's to Jacopo Baboni-Schilingi, Giacomo Platini, Paolo Aralla, Killian Sprotte, Julien Vincenot, Carlo Ciceri...

;;;; LOAD IN ASDF :
;;;; (asdf:load-system :fv-morphologie)


;;;; TODO
;;;; struct ---> pourcentage de completion !!!!
;;;; adding a som somewhere in classification - with save weigths then analysing weigth (> tutorial) ?
;;;; ...


;;; Loading fv-morphologie
(in-package :fv-morphologie)

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
  (float (/ (apply #'+ data) (length data))))

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
  (let ((sum (apply #'+ data)))
    (if (= 1 sum) data
    (mapcar #'(lambda (x) (/ x sum)) data))))


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


;;; ABOUT LISTS

(defun lmin (liste)
  (let ((a (nth (random (length liste)) liste)))
    (loop until (null liste)
       do (let ((e (pop liste)))
	    (when (< e a) (setf a e))))
    (values a)))

(defun lmax (liste)
  (let ((a (nth (random (length liste)) liste)))
    (loop until (null liste)
          do (let ((e (pop liste)))
	       (when (> e a) (setf a e))))
    (values a)))

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
	       (read-from-string (concat (mapcar #'(lambda (x) (format nil "~S" x))
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

(defgeneric fnoise (list &optional mode val)
 (:documentation "noise based on variation"))

(defun random-positive (q)
  "A random float in ]0, q[."
  (loop for r = (random (float q)) until (plusp r) finally (return r)))

(defmethod fnoise ((list list) &optional mode val)
  "Noise on list according to mode, with amount val (0.1 by default):
:nozero replaces each zero by a random value in ]0, val * range[ (]0, val[ if all values are equal);
:swap swaps val * length pairs of elements (val integer: number of swaps);
:relative adds to each value a random amount in [-val * range, val * range].
The range is max - min of list. A list which is not numerical is swapped (:swap).
If mode is a function of two arguments (x l), each element x of list is replaced by its result,
l being (a copy of) the whole list, shared by all the calls (it should not be modified)."
  (let ((val (or val 0.1)))
    (cond ((functionp mode)
           (let ((l (copy-list list)))
             (mapcar #'(lambda (x) (funcall mode x l)) list)))
          ((null list) nil)
          ((notevery #'numberp list) (randsort list val))
          (t (let ((range (- (reduce #'max list) (reduce #'min list))))
               (case mode
                 (:nozero
                  (let ((q (if (plusp range) (* val range) val)))
                    (unless (plusp q)
                      (error "filt-noise :nozero: val must be positive, not ~S." val))
                    (mapcar #'(lambda (x) (if (zerop x) (random-positive q) x)) list)))
                 (:swap (randsort list val))
                 (:relative
                  (let ((q (float (* val range))))
                    (mapcar #'(lambda (x) (+ x (* q (- (random 2.0) 1.0)))) list)))
                 (t (error "filt-noise: unknown mode ~S, use :nozero, :swap, :relative or a function." mode))))))))

#|
; deep-tree to do
(defmethod fnoise ((x list) &optional (quasi-zero .001))
  (if (null x) nil
    (if (numberp x)
        (a-zero x quasi-zero)
      (cons (a-zero (first x) quasi-zero)
            (a-zero (rest x) quasi-zero)))))
|#

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

(defun remove-local-dup-rec (liste test r) ; ex ptrn-smooth
  "Returns the list LIST without local repetitions (r: elements already kept, most recent first).
nil is an element like any other."
  (let ((started (and r t))
        (last (car r)))
    (dolist (x liste (nreverse r))
      (unless (and started (funcall test x last))
        (push x r)
        (setf last x started t)))))

(defgeneric rem-local-rep (seq &optional test)
  (:documentation "Removes local repetitions.
 A condition may be specified with optional argument test (default: equalp)."))

(defmethod rem-local-rep ((seq list) &optional (test #'equalp))
  (remove-local-dup-rec seq test nil))

(defmethod rem-local-rep ((seq t) &optional test)
  (declare (ignore test)) 
  seq)

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
  "max value of s"
  (let ((s (copy-tree seq)))
    (if (cdr s)
        (let ((x (pop (cdr s))))
          (if (> (car s) x)
              (fmax (cons (car s) (cdr s)))
            (fmax (cons x (cdr s)))))
      (car s))))

(defun fmin (seq)
  (let ((s (copy-tree seq)))
    (if (cdr s)
        (let ((x (pop (cdr s))))
          (if (< (car s) x)
              (fmin (cons (car s) (cdr s)))
            (fmin (cons x (cdr s)))))
	(car s))))

; fonction generique pour appliquer une fonction arbitraire sur une liste selon une fenetre glissante de taille (plus ou moins) determinee (pouvant varier dynamiquement ?) remplace 

(defun window-block (result v start end bstart bend)
  "The values given for the block [bstart, bend) of v by the result of a function applied
to the window [start, end): an atom (or nil) is repeated for the block; a list replaces
the block if the window is the block, otherwise it must be as long as the window and
gives the values at the positions of the block."
  (cond ((atom result) (make-list (- bend bstart) :initial-element result))
        ((and (= start bstart) (= end bend)) (copy-list result))
        ((= (length result) (- end start))
         (loop for p from bstart below bend
               collect (if (and (>= p start) (< p end))
                           (nth (- p start) result)
                           (aref v p))))
        (t (error "The function returned a list of ~D values for a window of ~D values."
                  (length result) (- end start)))))

(defun window-apply (seq func wind &optional (step 1))
  "Applies func to a window of wind values of seq.
With step 1, the window is centred on each value (an even wind is enlarged to the next
odd size) and shrinks symmetrically at the edges, so the first and last values are kept.
With a larger step, func is applied to a window of wind values centred on each block of
step values. See window-block for the values given by the result of func."
  (let* ((v (coerce seq 'vector))
         (l (length v))
         (wind (max 1 wind))
         (step (max 1 step)))
    (flet ((apply-to (start end bstart bend)
             (window-block (funcall func (coerce (subseq v start end) 'list))
                           v start end bstart bend)))
      (if (= step 1)
          (let ((h (floor (if (evenp wind) (1+ wind) wind) 2)))
            (loop for i below l
                  append (let ((k (min h i (- l 1 i))))
                           (apply-to (- i k) (+ i k 1) i (1+ i)))))
          (loop for n from 0 below l by step
                append (let* ((start (max 0 (- n (floor (- wind step) 2))))
                              (end (min l (+ start wind))))
                         (apply-to start end n (min l (+ n step)))))))))

;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'mean 3 3)
;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'mean 4 1)
;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'mean 4)
;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'(lambda (x) (nth (random (length x)) x)) 4)
;(window-apply '(1 2 3 4 5 6 7 8 9 10 11 12) #'(lambda (x) (loop repeat (length x) collect (nth (random (length x)) x))) 2 2)

;(defun resample (data n &optional mode)
;  (let ((l (length data)))
;    ))

(defun resample-binary (data n)
  (let ((l (length data))
        (r (list)))
    (if (= n l)
        data  (let ((k (/ l n)))
		(dotimes (a n (nreverse r))
		  (push (nth (min (round (* a k)) (1- l)) data) r))))))

(defun median-simple (list w)
  "Sliding median over w values centred on each value of list, w odd: the median is always a value of list.
At the edges, the first and last values are repeated. The result has the same length as list."
  (let* ((v (coerce list 'vector))
         (n (length v))
         (h (floor w 2)))
    (loop for i below n
          collect (nth h (sort (loop for k from (- i h) to (+ i h)
                                     collect (aref v (max 0 (min (1- n) k))))
                               #'<)))))

(defun median (list w) ;; ???
  (let ((r nil) ) ; (i  (1+ (floor (/ w 2)))))
    (loop until (null list)
       do (let (z (window (loop repeat w collect (if list (pop list) (car r)))))
            (setf z (sort window '<))
            (setf r (append r (remove-if #'(lambda (x) (or (= (car z) x) (= (car (last z)) x))) window)))))
    (reverse r)))

(defgeneric median-filter (seq window)
  (:documentation "Median filter."))

(defmethod median-filter ((seq list) (window integer))
  "Median filter over an odd window: an even window is enlarged to the next odd size."
  (if (< window 2)
      (copy-list seq)
      (median-simple seq (if (evenp window) (1+ window) window))))

(defmethod median-filter ((seq list) (window real))
  (median-filter seq (round window)))

(defun exponential-smoothing (data alpha)
  "s(t) = s(t-1) + alpha (x(t) - s(t-1)), s(0) = x(0)."
  (when data
    (let ((r (list (car data))))
      (dolist (x (cdr data) (reverse r))
        (push (+ (car r) (* alpha (- x (car r)))) r)))))

(defun double-exponential-smoothing (data alpha gamma)
  "Holt's double exponential smoothing, with level s and trend b:
s(t) = alpha x(t) + (1 - alpha) (s(t-1) + b(t-1))
b(t) = gamma (s(t) - s(t-1)) + (1 - gamma) b(t-1)
from s(0) = x(0) and b(0) = x(1) - x(0)."
  (if (null (cdr data))
      (copy-list data)
      (let* ((s (car data))
             (b (- (cadr data) (car data)))
             (r (list s)))
        (dolist (x (cdr data) (nreverse r))
          (let ((s1 (+ (* alpha x) (* (- 1 alpha) (+ s b)))))
            (setf b (+ (* gamma (- s1 s)) (* (- 1 gamma) b))
                  s s1)
            (push s r))))))

;;filtre extreme (contraire de median)

(defun extrema (list w &optional (minmax :max))
  (let ((r nil)) ; (i  (1+ (floor (/ w 2)))))
    (loop until (null list)
       do (let ((window (loop for k from 0 to w
			   collect (let ((z (nth k list))) (if z z (car r))))))
            (case minmax
              (:max (push (car (sort window '>)) r))
              (:min (push (car (sort window '<)) r))
              (t (push (car (sort window '>)) r))))
	 (pop list))
    (nreverse r)))

(defgeneric extrema-filter (seq window)
  (:documentation "Extrema filter."))

(defmethod extrema-filter ((seq list) (window integer))
  (if (<= (length seq) window)
      seq (values (extrema seq window))))

(defmethod extrema-filter ((seq list) (window null))
  (values (extrema seq (max 3 (floor (/ (length seq) 100))))))

;; TOOLS for NUMERIC SERIES
;; primitives are contour features - ie signatures or landmarks - (forme of changes of direction) in a sequence of numeric values ;
;; a primitive is made of 3 dots defining 2 segments combining ascending, descending or inflexion contour...

(defun next-sign (seq &optional (state 0) (i 0))
  "Returns next local extrema : position sign value"
  (if seq
      (let ((a (pop seq)))
        (if seq
            (let ((b (car seq)))
              (if (cdr seq)
                  (let* ((c (cadr seq))
                         (d (- b a))
                         (e (- c b)))
                    (setf d (if (zerop d) 0 (if (plusp d) 1 -1))
                          e (if (zerop e) 0 (if (plusp e) 1 -1)))
                    (if (zerop e)
                        (if (zerop d)
                            (list (1+ i) 0 b)
                          (next-sign seq state (1+ i)))
                      (if (and (not (zerop state)) (not (= state e)))
                          (list (1+ i) e b)
                        (if (plusp d)
                            (if (plusp e)
                                (next-sign seq 1 (1+ i))
                              (if (minusp e)
                                  (list (1+ i) -1 (car seq))
                                (next-sign seq 1 (1+ i))))
                          (if (minusp d)
                              (if (plusp e)
                                  (list (1+ i) 1 (car seq))
                                (if (minusp e)
                                    (next-sign seq -1 (1+ i))
				    (next-sign seq -1 (1+ i))))
			      (next-sign seq e (1+ i))))))) nil)) nil)) nil))

#|
(next-sign '(11 12 13 13 13 12 11))
(next-sign '(11 12 12 12 13 13 13 12 11))
(next-sign '(11 12 13 13 12 11))
(next-sign '(13 12 11 11 11 12 13))
(next-sign '(13 12 12 12 11 11 11 12 13))
(next-sign '(13 12 11 11 12 13))
(next-sign '(11 11 12 13 14))
(next-sign '(13 12 12 12 11 11))
|#

(defun derivee (seq seuil-a seuil-f)
  (let (r tmp (i 0))
    (loop until (not i) do
          (when (setf tmp (next-sign (subseq seq i) 0 i))
            (let ((previous (position-if #'(lambda (x) (or t ;(zerop (cadr x))
                                                           (= (* (cadr x) -1) (cadr tmp)))) r))
                  (next (if tmp (next-sign (subseq seq (1+ (car tmp))) 0 (1+ (car tmp))) nil)))
              ;(print (list i tmp (if previous (nth previous r) (nth i seq)) (if next next nil)))
              (if (or (>= (abs (- (caddr tmp) (if previous (caddr (nth previous r)) (nth i seq)))) seuil-a)
                      (if (and next t) ;(= (cadr tmp) (cadr next)))
                          nil (>= (abs (- (caddr tmp) (if next (nth (car next) seq) (nth i seq)))) seuil-a)))
		  (if r (if (zerop (cadar r))
                            (when (not (zerop (cadr tmp)))
                              (push tmp r))
			    (push tmp r))
                    (push tmp r))
                (when (>= (- (car tmp) (if r (caar r) 0)) seuil-f)
                  (if r (when (not (zerop (cadar r)))
                          (push (list (car tmp) 0 (caddr tmp)) r))
                    (push (list (car tmp) 0 (caddr tmp)) r))))))
          (setf i (car tmp)))
    (loop for e in r do (setf (cadr e) (* -1 (cadr e))))
    (reverse r)))

(defun sign (x &optional thres)
  (if (not thres)
      (if (zerop x) 0 (if (plusp x) 1 -1))
    (if (<= (abs x) thres) 0 (if (plusp x) 1 -1))))

(defun primitive-value (triplet &optional thres)
  (let ((a (sign (- (cadr triplet) (car triplet)) thres))
        (b (sign (- (caddr triplet) (cadr triplet)) thres)))
    (if (or (zerop a) (zerop b))  ;; and or or or xor ?
	0 (cond ((and (plusp a) (minusp b)) 1)
		((and (minusp a) (plusp b)) -1)
            (t nil)))))
#|
(defun primitive-value (triplet &optional thres)
  (let ((a (sign (- (cadr triplet) (car triplet)) thres))
        (b (sign (- (caddr triplet) (cadr triplet)) thres)))
    (cond ((and (zerop a) (zerop b)) 0)
          ((and (zerop a) (not (zerop b))) 0)
          ((and (plusp a) (zerop b)) 1)
          ((and (minusp a) (zerop b)) -1)
          ((and (plusp a) (minusp b)) 1)
          ((and (minusp a) (plusp b)) -1)
          (t (print 'x) nil))))
|#

(defun primitive-sign (a b thres) ; a ou b = (pos primitive-value value)
  (if b  ; b may be nil in some cases
      (sign (- (caddr b) (caddr a)) thres)
					;(* (cadr a) (cadr b))		
      (cadr a)))

(defun mfm (seq i)
  (if seq (let ((p (cadar seq)))
            (if (zerop p)
                (mfm (cdr seq) (1+ i))
              i)) i))

;(mfm '((1 0 1) (2 0 2) (3 0 3) (4 0 4)) 0)

(defun primitives-nothres (seq)
  (let ((lm))
    (loop for i from 0 to (- (length seq) 3) do
          (let ((k (primitive-value (subseq seq i (+ i 3)))))
            (when k
              (push (list (1+ i) k (nth (1+ i) seq)) lm))))
    (setf lm (nreverse lm))
    (loop for i from 0 to (- (length lm) 2) do
          (let ((p (nth (1+ i) lm))
                (p-1 (nth i lm))
                (n 0))
            (when (zerop (cadr p))
              (setf n (mfm (subseq lm i) 1))
              (when  (nth (+ i n 1) lm)
                (when (= (cadr p-1) (cadr (nth (+ i n 1) lm)))
                  (setf (cadr p) (* -1 (cadr p-1))))
                (incf i n)))) )
    (values lm)))

(defun primitives-raw (seq thres)
  (if (or (zerop thres) (not thres))
      (primitives-nothres seq)
    (let ((lm (primitives-nothres seq)))
      (loop for i from 0 to (- (length lm) 2) do
            (let ((a (nth i lm))
                  (b (nth (1+ i) lm)))
              (when (zerop (primitive-sign a b thres))
                ;(setf (nth i lm) (list (car a) 0 (caddr a))) )))
                (setf (nth (1+ i) lm) (list (car b) 0 (caddr b))))))
      (values lm))))

(defun notzerop (x)
  (if x (not (zerop x)) nil))

(defun primitives-thres (seq thres &optional (flex-thres 2))
  (let ((lm (primitives-raw seq thres))
        (ft (max flex-thres 2))
        cime r)
    (if (or (zerop thres) (not thres)) lm
      (progn
        (loop until (null lm) do
              (let ((k (pop lm)))
                (if (zerop (cadr k))
                    (progn
                      (setf cime (list k))
                      (loop until (or (notzerop (primitive-sign (car cime) (car lm) thres))
                                      (not lm)) do
                            (push (pop lm) cime))
                      ;(print cime)
                      (if (eq (cadar r) (cadar lm))  ; m f f m > m M m / M f f M > M m M
                          (if (> (length cime) ft)
                              (let* ((vn (cadar lm))
                                     (max (if (plusp vn)
                                              (car (sort cime '< :key #'caddr))
                                            (car (sort cime '> :key #'caddr)))))
                                (setf (cadr max) (* -1 vn))
                                (push max r))
                              (setf r (cdr r)))
                        (if (> (length cime) ft)
                            (let ((mean (float (/ (apply #'+ (mapcar #'caddr cime)) (length cime)))))
                              (mapcar #'(lambda (x) (setf (caddr x) mean)) cime)
                              (setf r (append (list (car cime) (car (last cime))) r)))
                          (let* ((vn (cadar r))
                                 (max (if (plusp vn)
                                          (car (sort cime '> :key #'caddr))
                                        (car (sort cime '< :key #'caddr)))))
                            (if (plusp vn)
                                (when (< (caddar r) (caddr max))
                                  (setf (cadr max) (* 1 vn))
                                    (setf (car r) max))
                              (when (> (caddar r) (caddr max))
                                (setf (cadr max) (* 1 vn))
                                (setf (car r) max))) ))))
                  (push k r))))
        (values (nreverse r))))))

(defun primitives (seq thres &optional (mode t) (inflex-thres 2))
  (let ((lm (primitives-thres seq thres inflex-thres)))
    (if mode lm (remove-if #'(lambda (x) (zerop (cadr x))) lm))))


(defun flat-primitives (seq thres &optional (mode t) (inflex-thres 2))
  "primitives of seq where each run of equal values (plateau) counts as a single point, at its first position:
a flat extremum is given once, as a minimum or a maximum. With mode t (inflexions), a plateau which is
not an extremum gives one inflexion, at its first position."
  (let* ((v (coerce seq 'vector))
         (last (1- (length v)))
         (runs (loop with i = 0
                     while (<= i last)
                     collect (let ((j (loop for k from i
                                            while (and (< k last) (= (aref v (1+ k)) (aref v i)))
                                            finally (return k))))
                               (prog1 (list i (aref v i) (1+ (- j i)))
                                 (setf i (1+ j))))))
         (lm (mapcar #'(lambda (p) (list (first (nth (first p) runs)) (second p) (third p)))
                     (primitives (mapcar #'second runs) thres mode inflex-thres))))
    (when mode
      (dolist (run runs)
        (destructuring-bind (i value n) run
          (when (and (> n 1) (< 0 i last) (not (assoc i lm)))
            (push (list i 0 value) lm)))))
    (sort lm #'< :key #'first)))

(defun major-extrema (seq r &optional (dist #'(lambda (a b) (abs (- a b)))))
  "Important extrema of seq (Fink & Gandhi, 2007): a_m is an important minimum if there are i < m < j
such that a_m is the minimum of a_i..a_j, dist(a_m, a_i) >= r and dist(a_m, a_j) >= r (and so for maxima).
With r = 0, every strict local extremum. Linear algorithm of Fink & Gandhi; returns the inner extrema
as (position type value), type -1 for a minimum and 1 for a maximum (the endpoints are not returned)."
  (let* ((v (coerce seq 'vector))
         (last (1- (length v)))
         (out nil))
    (labels ((near (a b) (let ((d (funcall dist a b))) (if (zerop r) (<= d 0) (< d r))))
             (emit (i type) (when (< 0 i last) (push (list i type (aref v i)) out)))
             (find-first ()
               (let ((i 0) (imin 0) (imax 0))
                 (loop while (and (< i last)
                                  (near (aref v i) (aref v imax))
                                  (near (aref v i) (aref v imin)))
                       do (incf i)
                          (when (< (aref v i) (aref v imin)) (setf imin i))
                          (when (> (aref v i) (aref v imax)) (setf imax i)))
                 (if (< imin imax) (emit imin -1) (emit imax 1))
                 i))
             (find-minimum (i)
               (let ((imin i))
                 (loop while (and (< i last) (near (aref v i) (aref v imin)))
                       do (incf i)
                          (when (< (aref v i) (aref v imin)) (setf imin i)))
                 (when (or (< i last) (< (aref v imin) (aref v i))) (emit imin -1))
                 i))
             (find-maximum (i)
               (let ((imax i))
                 (loop while (and (< i last) (near (aref v imax) (aref v i)))
                       do (incf i)
                          (when (> (aref v i) (aref v imax)) (setf imax i)))
                 (when (or (< i last) (> (aref v imax) (aref v i))) (emit imax 1))
                 i)))
      (when (> last 0)
        (let ((i (find-first)))
          (when (and (< i last) (< (aref v i) (aref v 0)))
            (setf i (find-minimum i)))
          (loop while (< i last)
                do (setf i (find-maximum i))
                   (when (< i last) (setf i (find-minimum i))))))
      (nreverse out))))

(defun local-extrema (seq)
  "The inner local extrema of seq, where the variation changes direction, as (position type value):
type -1 for a minimum, 1 for a maximum. A flat extremum is given at its first position."
  (let* ((v (coerce seq 'vector))
         (l (length v))
         (out nil)
         (dir 0)       ; direction of the last variation: -1, 0 (none yet) or 1
         (start 0))    ; first position of the current run of equal values
    (loop for i from 1 below l
          do (let ((d (signum (- (aref v i) (aref v (1- i))))))
               (unless (zerop d)
                 (when (and (/= dir 0) (/= d dir) (> start 0))
                   (push (list start (if (> dir 0) 1 -1) (aref v start)) out))
                 (setf dir d))
               (when (/= (aref v i) (aref v (1- i))) (setf start i))))
    (nreverse out)))

(defun landmarks (seq thres &optional option)
  "Landmarks of seq (Perng, Parker & Leung, 1999): the first-order landmarks (local extrema), smoothed
by MDPP: a pair of consecutive landmarks (x1 y1) (x2 y2) is removed while x2 - x1 < D and their
difference is below thres. option is nil (no D, difference |y2 - y1|), a number (D), a function (a b)
giving the difference, or a list (D criterion), criterion being :absolute (|y2 - y1|), :relative
(2 |y2 - y1| / (|y1| + |y2|), as in Perng et al.) or a function.
Returns (position type value), type -1 for a minimum and 1 for a maximum."
  (multiple-value-bind (d criterion)
      (cond ((null option) (values nil :absolute))
            ((realp option) (values option :absolute))
            ((functionp option) (values nil option))
            ((listp option) (values (first option) (or (second option) :absolute)))
            (t (error "landmarks: option must be nil, a distance, a function or a list (distance criterion), not ~S." option)))
    (let ((diff (cond ((eq criterion :absolute) #'(lambda (a b) (abs (- b a))))
                      ((eq criterion :relative)
                       #'(lambda (a b)
                           (let ((m (+ (abs a) (abs b))))
                             (if (zerop m) 0 (/ (* 2 (abs (- b a))) m)))))
                      ((functionp criterion) criterion)
                      (t (error "landmarks: criterion must be :absolute, :relative or a function, not ~S." criterion))))
          (lm (local-extrema seq)))
      (flet ((removable (a b)
               (and (or (null d) (< (- (first b) (first a)) d))
                    (< (funcall diff (third a) (third b)) thres))))
        (loop for k = (loop for (a b) on lm
                            for i from 0
                            when (and b (removable a b)) return i)
              while k
              do (setf lm (append (subseq lm 0 k) (nthcdr (+ k 2) lm))))
        lm))))

(defgeneric signature (seq mode thres &optional option)
  (:documentation
"Signature of a sequence with some threshold <thres> for variation in intensity.
The argument <thres> may be nil or 0 (no threshold for variations in intensity), or an absolute amplitude (in the unit of the values of <seq>): variations smaller than <thres> are ignored.
 Argument <mode> define the algorithm to compute signature which may require the optional argument <option> :
:miflexnmax, returns minima, maxima and inflexions using Fred Voisin pragmatic method.
:minmax, same with only maxima and minima (without inflexions);
:major-extrema, important extrema (Fink & Gandhi, Conference on Systems, Man and Cybernetics, 2007):
 a minimum (maximum) such that the sequence rises (falls) by at least thres on both sides;
:landmarks, first-order landmarks smoothed by MDPP(D, P) (Perng, Parker & Leung, Conference on Information
 and Knowledge Management, 1999): pairs of close landmarks whose difference is below thres are removed.
Any other method is an error.
Argument <thres> is the treshold value (delta) in amplitude.
Argument <option> is an optional parameter depending to the choosen method :
with :minflexmax method, <option> is the time (rank) distance threshold for detecting inflexions (a nul or positive number);
with :major-extrema, <option> may be a distance function (a b) for comparing values (|a - b| by default);
with :landmarks, <option> is nil, a number D (only landmarks closer than D ranks may be removed), a function (a b) giving the
difference between two values (|a - b| by default), or a list (D criterion), criterion being :absolute, :relative
(2 |a - b| / (|a| + |b|), as in Perng et al.) or a function."))

(defmethod signature ((seq null) (mode t) (thres t) &optional (option nil))
  (declare (ignore option))
  nil)

(defmethod signature ((seq list) (mode t) (thres number) &optional (option nil))
  (let* ((max (lmax seq))
         (min (lmin seq))
         (thres1 (if thres thres 0.0))
         (thres2 (if (integerp option) option (floor (* (- max min) (if (numberp option) option 0) (length seq)))))) ;) (* y x th)
    (case mode
      (:minmax (flat-primitives seq thres1 nil thres2))
      (:minflexmax (flat-primitives seq thres1 t thres2))
      (:major-extrema (major-extrema seq thres1
                                     (if (functionp option) option #'(lambda (a b) (abs (- a b))))))
      (:landmarks (landmarks seq thres1 option))
      (t (error "signature: unknown method ~S, use :minmax, :minflexmax, :major-extrema or :landmarks." mode)))))

(defmethod signature ((seq list) (mode t) (thres null) &optional (option nil))
  (signature seq mode 0 option))

(defun droite (a b)
  (if (= (cadr a) (cadr b))
      (list 0 (cadr a))
    (let ((pente (/ (- (cadr a) (cadr b)) (- (car a) (car b)))))
      (list pente (- (cadr a) (* pente (car a)))))))

(defun echantillonage (seq method mode thres option)
  "The points of the signature of seq, with its first and last values, according to mode
(:values, :positions, :all or :resampled)."
  (unless (member mode '(:values :positions :all :resampled))
    (error "exsample: unknown mode ~S, use :values, :positions, :all or :resampled." mode))
  (let ((pos (remove-duplicates
              (append (list 0)
                      (mapcar #'car (signature seq method thres option))
                      (list (1- (length seq)))))))
    (case mode
      (:values
       (mapcar #'(lambda (x) (nth x seq)) pos))
      (:positions pos)
      (:all
       (mapcar #'(lambda (x) (list x (nth x seq))) pos))
      (:resampled
       (let ((r (list)))
         (loop for i from 0 to (- (length pos) 2) do
               (let* ((xa (nth i pos))
                      (xb (nth (1+ i) pos))
                      (steps (- xb xa 1))
                      (ya (nth xa seq))
                      (yb (nth xb seq))
                      (d (droite (list xa ya) (list xb yb))))
                 (push ya r)
                 (when (> steps 0)
                   (loop for j from 1 to steps do
                         (push (+ (* (car d) (+ xa j)) (cadr d)) r)))))
         (append (nreverse r) (last seq)))))))

;;; DISTANCES DISSEMBLANCE ETC

;; 1. Distance euclidienne

(defgeneric dist-euclid (a b &optional key)
  (:documentation "Euclidian distance between points a and b.
Point coordinates are represented by a list of values in any euclidian space.
 if a is a list of coordinates (lists) and b null,
output is the list of distances for all points to all others ;
 if a or b is a number, consider b or a as list of numbers (1 dim).
If a and b are lists with not same size (i.e points with not same dimensions),
 returns NIL (no distance)."))

(defmethod dist-euclid ((a number) (b number) &optional (key #'identity))
  (declare (ignore key))
  (abs (- b a)))

(defmethod dist-euclid ((a list) (b number) &optional (key #'identity))
  ;(assert (not (member 'nil (mapcar #'numberp a))))
  (mapcar #'(lambda (x) (dist-euclid x b key)) a))

(defmethod dist-euclid ((a number) (b list) &optional (key #'identity))
  ;(assert (not (member 'nil (mapcar #'numberp b))))
  (mapcar #'(lambda (x) (dist-euclid a x key)) b))

(defmethod dist-euclid ((a list) (b list) &optional (key #'identity))
  ;(assert (equalp (mapcar #'numberp a) (mapcar #'numberp b)))
  (if key
      (sqrt (apply #'+ (loop for i in (funcall key a) for j in (funcall key b) collect (expt (- j i) 2))))
    (sqrt (apply #'+ (loop for i in a for j in b collect (expt (- j i) 2))))
    ))

(defmethod dist-euclid ((a list) (b null) &optional (key #'identity))
  ;(assert (apply #'= (mapcar #'length a)))
  (let ((r (list)))
    (dotimes (i (length a) (nreverse r))
      (loop for j from (1+ i) to (1- (length a))
            do (push (list i j (dist-euclid (nth i a) (nth j a) key)) r)))))

(defgeneric city-bloc (a b)
  (:documentation "City-bloc distance between a and b in euclidian space with n dimensions (with integer n > 0)."))

(defmethod city-bloc ((a number) (b number))
 (if (> a b) (- a b) (- b a)))

(defmethod city-bloc ((a list) (b list))
  (assert (= (length a) (length b)))
  (apply #'+ (mapcar #'(lambda (x y) (city-bloc x y)) a b)))

(defmethod city-bloc ((a list) (b null))
  (assert (apply #'= (mapcar #'length a)))
  (let ((r (list)))
    (dotimes (i (length a) (nreverse (remove 'nil r)))
      (loop for j from (1+ i) to (1- (length a)) do
	 (push (list i j (city-bloc (nth i a) (nth j a))) r)))))

(defgeneric close-p (a b thresh &optional dist)
  (:documentation "Tests if a and b are close according to their distance."))

(defmethod close-p ((a list) (b list) (thresh number) &optional (dist #'dist-euclid))
  (let ((d (funcall dist a b)))
    (when d (<= d thresh))))

(defmethod close-p ((a number) (b number) (thresh number) &optional (dist #'dist-euclid))
  (let ((d (funcall dist a b)))
    (when d (<= d thresh))))

(defmethod close-p ((a null) (b null) (thresh number) &optional (dist #'dist-euclid))
  (declare (ignore dist))
  nil)

(defmethod close-p ((a t) (b null) (thresh number) &optional (dist #'dist-euclid))
  (declare (ignore dist))
  nil)

(defmethod close-p ((a null) (b t) (thresh number) &optional (dist #'dist-euclid))
  (declare (ignore dist))
  nil)

; (close-p '(1.1 1.9) '(1 2) .5)


(defun hamming-distance (v1 v2 &optional (test #'eq))
  (assert (= (length v1) (length v2)))
  (let ((d 0))
    (dotimes (i (length v1) d)
      (when (not (funcall test (nth i v1) (nth i v2)))
        (incf d)))))

(defgeneric hamming-dist (v1 v2 &optional norm test)
  (:documentation "Hamming distance."))

(defmethod hamming-dist ((v1 list) (v2 list) &optional (norm t) (test #'eq))
  (let ((d (hamming-distance v1 v2 test)))
    (if norm (float (/ d (length v1))) d)))

(defmethod hamming-dist ((v1 list) (v2 null) &optional (norm t) (test #'eq))
  (let ((r (list)))
    (dotimes (i (1- (length v1)) (nreverse r))
      (dotimes (j (- (length v1) i 1))
        (push (list i (+ 1 j i) (hamming-dist (nth i v1) (nth (+ 1 j i) v1) norm test)) r)))))

;; 2. 'Editing distance' (Distance d'edition)
;; I have red a paper on genomic analysis on ADN using, as I remember, also used by a unix text commande.
;; Finally, as I understand it now, it's a kind of measure of the work of a Turing Machine:
;; the less it does, the closest it is...
;; (I may write here a Turing machine here to do the same :)

(defun edit-dist1 (seq1 seq2 &key (change 1) (insert 1) (delete 1) (normalized nil) (test #'equalp))
"Smallest distance between two lists of symbols.
 Keywords arguments:
  change: cost if changing a elt in sequences;
  insert: cost if deleting or inserting;
  normalized (T or NIL): normalized distance - 0 the closest, 1 the farest."
  (let* ((a (coerce seq1 'vector))
         (b (coerce seq2 'vector))
         (la (length a))
         (lb (length b))
         (prev (make-array (1+ la)))
         (row (make-array (1+ la))))
    ;; row j of the matrix of costs: (aref row i) for the first i elements of seq1, j of seq2
    (dotimes (i (1+ la)) (setf (aref prev i) (* i delete)))
    (loop for j from 1 to lb
          do (setf (aref row 0) (+ (aref prev 0) insert))
             (loop for i from 1 to la
                   do (setf (aref row i)
                            (min (+ (aref prev (1- i))
                                    (if (funcall test (aref a (1- i)) (aref b (1- j))) 0 change))
                                 (+ (aref prev i) insert)
                                 (+ (aref row (1- i)) delete))))
             (rotatef prev row))
    (let ((d (aref prev la)))
      (if normalized
          (if (zerop (max la lb)) 0.0 (float (/ d (max la lb))))
          d))))

(defun edit-dist2 (seq1 seq2 &key (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  "Returns the smallest distance between two lists of symbols.
 Keywords arguments:
  change: cost if changing a elt in sequences;
  insert: cost if deleting or inserting;
  inex = cost if changing with an element which doesn't exist on the other sequence;
  normalized (T or NIL): normalized distance, 0 the closest, 1 the farest."
  (let* ((a (coerce seq1 'vector))
         (b (coerce seq2 'vector))
         (la (length a))
         (lb (length b))
         ;; 1 for an element which does not exist (according to test) in the other sequence
         (ex (map 'vector #'(lambda (x) (if (member x seq2 :test test) 0 1)) a))
         (ey (map 'vector #'(lambda (y) (if (member y seq1 :test test) 0 1)) b))
         (prev (make-array (1+ la)))
         (row (make-array (1+ la))))
    (setf (aref prev 0) 0)
    (loop for i from 1 to la
          do (setf (aref prev i) (+ (aref prev (1- i)) delete (* (aref ex (1- i)) inex))))
    (loop for j from 1 to lb
          do (setf (aref row 0) (+ (aref prev 0) insert (* (aref ey (1- j)) inex)))
             (loop for i from 1 to la
                   do (let ((exi (aref ex (1- i))) (eyj (aref ey (1- j))))
                        (setf (aref row i)
                              (min (+ (aref prev (1- i))
                                      (if (funcall test (aref a (1- i)) (aref b (1- j)))
                                          0
                                          (+ change (* (+ exi eyj) inex))))
                                   (+ (aref prev i) insert (* eyj inex))
                                   (+ (aref row (1- i)) delete (* exi inex))))))
             (rotatef prev row))
    (let ((d (aref prev la)))
      (if normalized
          (if (zerop (max la lb)) 0.0 (float (/ d (max la lb))))
          d))))

(defun editing-dist (seq1 seq2 change insert delete inex normalized test)
"Editing distance between sequences of symbols.
 seq1: first sequence;
 seq2: second sequence;
 change: cost for changing a symbol;
 insert: cost for deleting or inserting a symbol;
 inex: cost for changig with a symbol from seq2 which doesn't exist in seq1;
 normalized: normalized output, in ratio to the length of the shortest sequence;
 test: any binary function to compare symbols (default: #'equalp)."
 (if (not inex)
     (edit-dist1 seq1 seq2 :change change :insert insert :delete delete :normalized normalized :test test)
     (if (zerop inex)
	 (edit-dist1 seq1 seq2 :change change :insert insert :delete delete :normalized normalized :test test)
	 (edit-dist2 seq1 seq2 :change change :insert insert :delete delete :inex inex :normalized normalized :test test))))

(defgeneric edit-dist (seq1 seq2 &optional change insert delete inex normalized test)
  (:documentation
"Dissemblance measurement between sequences of symbols <seq1> and <seq2>, according to the 'editing distance';
 the dissemblance is nul (zero) when the sequences are the considered to be the same or 'equal'.
 Argument <change> is the cost value when changing a symbol ;
 Argument <insert> is the cost value when inserting a symbol ;
 Argument <delete> is the cost value when deleting a symbol ;
 Argument <inex> is the cost value when changing a symbol in one of the sequence which does not exist in the other ;
 Argument <normalized> to choose (un-)normalized output. If normalized, value 1.0 in ratio to the length of the shortest sequence to be compared;
 test: any binary function to compare symbols (default: #'equalp)."))

(defmethod edit-dist ((seq1 list) (seq2 list) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  ;(if (not (member t (mapcar #'listp seq2)))
  ;    (mapcar #'(lambda (a b) (edit-dist 
  (editing-dist seq1 seq2 change insert delete inex normalized test))

(defmethod edit-dist ((seq1 list) (seq2 null) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (declare (ignore seq2))
  (when (not (= (length seq1)
		(length (remove-duplicates (copy-tree seq1) :test #'equalp))))
    (message "gen-dist: warning ! duplicates in input list."))
  (let ((r nil) (l (length seq1)))
    (dotimes (i l (nreverse r))
      (dotimes (j (- l i 1))
        (push (list i (+ i j 1) (edit-dist (nth i seq1) (nth (+ i j 1) seq1) change insert delete inex normalized test)) r))) ))

(defmethod edit-dist ((seq1 string) (seq2 string) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (string2code seq1) (string2code seq2) change insert delete inex normalized test))

(defmethod edit-dist ((seq1 symbol) (seq2 symbol) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (alpha>num seq1) (alpha>num seq2) change insert delete inex normalized test))

(defmethod edit-dist ((seq1 symbol) (seq2 number) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (alpha>num seq1) (alpha>num seq2) change insert delete inex normalized test))

(defmethod edit-dist ((seq1 number) (seq2 symbol) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (alpha>num seq1) (alpha>num seq2) change insert delete inex normalized test))

(defmethod edit-dist ((seq1 number) (seq2 number) &optional (change 1) (insert 1) (delete 1) (inex 0) (normalized nil) (test #'equalp))
  (edit-dist (alpha>num seq1) (alpha>num seq2) change insert delete inex normalized test))

(defun dimension-weights (wgth n)
  "The weights of n dimensions, summing to 1, from wgth: a number gives the same weight to all;
a list too short is completed with its last weight, a list too long is cut.
If all the weights are 0, all are 0 (every dimension is ignored)."
  (let ((w (cond ((numberp wgth) (make-list n :initial-element wgth))
                 ((< (length wgth) n)
                  (message (format nil "wgth has ~D weights for ~D dimensions: the last weight is used for the missing ones."
                                   (length wgth) n))
                  (append wgth (make-list (- n (length wgth)) :initial-element (or (car (last wgth)) 1))))
                 ((> (length wgth) n)
                  (message (format nil "wgth has ~D weights for ~D dimensions: the extra weights are ignored."
                                   (length wgth) n))
                  (subseq wgth 0 n))
                 (t wgth))))
    (if (zerop (apply #'+ w))
        w
        (normalize w))))

(defun edit-dist-ldl1 (seq1 seq2 &key (change 1) (insert 1) (delete 1) (wgth '(1 1)) (test #'equalp))
  (assert (and (apply #'= (mapcar #'length seq1))
	       (apply #'= (mapcar #'length seq2))
	       (= (length (car seq1)) (length (car seq2)))))
  (let* ((r 0)
	 (matrix1 (mat-transpose seq1))
	 (matrix2 (mat-transpose seq2))
	 (wgth (dimension-weights wgth (length (car seq1)))))
    (dotimes (n (length matrix1) r) ;(/ r (length (car seq1))))
      (incf r (* (nth n wgth)
		 (edit-dist1 (nth n matrix1) (nth n matrix2)
				   :change change :insert insert :delete delete :normalized 'T :test test))))))

(defun edit-dist-ldl2 (seq1 seq2 &key (change 1) (insert 1) (delete 1) (inex 0) (wgth '(1 1)) (test #'equalp))
  (assert (and (apply #'= (mapcar #'length seq1))
	       (apply #'= (mapcar #'length seq2))
	       (= (length (car seq1)) (length (car seq2)))))
  (let* ((r 0)
	 (matrix1 (mat-transpose seq1))
	 (matrix2 (mat-transpose seq2))
	 (wgth (dimension-weights wgth (length (car seq1)))))
    (dotimes (n (length matrix1) r) ;(/ r (length (car seq1))))
      (incf r (* (nth n wgth)
		 (edit-dist2 (nth n matrix1) (nth n matrix2)
			     :change change :insert insert :delete delete :inex inex :normalized 'T :test test))))))

(defgeneric multi-edit-dist (seq1 seq2 wgth &key change insert delete inex test)
  (:documentation
 "Considering each element of sequences seq1 and seq2 as an event described into symbolic dimensions,
returns the editing distance between seq1 and seq2 according to the weigthed dimensions defined.
By symbolic dimensions, I consider a structured list of 'arbitrary' symbols describing an event, such as, for instance,
 '(a IV x)."))

(defmethod multi-edit-dist ((seq1 list) (seq2 list) (wgth t)
			    &key (change 1) (insert 1) (delete 1) (inex 0) (test #'equalp))
  (if inex
      (if (zerop inex)
	  (edit-dist-ldl1 seq1 seq2 :change change :insert insert :delete delete :wgth wgth :test test)
	  (edit-dist-ldl2 seq1 seq2 :change change :insert insert :delete delete :inex inex :wgth wgth :test test))
      (edit-dist-ldl1 seq1 seq2 :change change :insert insert :delete delete :wgth wgth :test test)))

(defmethod multi-edit-dist ((seq1 list) (seq2 null) (wgth t)
			    &key (change 1) (insert 1) (delete 1) (inex 0) (test #'equalp))
  ;(assert (apply #'= (mapcar #'length seq1)))
  (let (r (l (length seq1)))
    (dotimes (i l (nreverse r))
      (dotimes (j (- l i 1))
        (push (list i (+ i j 1) (multi-edit-dist (nth i seq1) (nth (+ i j 1) seq1) wgth
                                                 :change change :insert insert :delete delete :inex inex :test test)) r)))))

(defgeneric commun-elt (set1 set2 &key test key)
  (:documentation "Returns common elements to both set1 and set2, nil if not."))

(defmethod commun-elt ((set1 list) (set2 list) &key test key)
  (when (not test) (setf test #'equalp))
  (when (not key) (setf key #'identity))
  (intersection set1 set2 :test test :key key))

(defmethod commun-elt ((set1 list) (set2 null) &key test key)
 (when (not test) (setf test #'equalp))
 (when (not key) (setf key #'identity))
 (let ((r nil) (l (length set1)))
  (dotimes (i l (nreverse r))
   (dotimes (j (- l i 1))
    (push (list i (+ i j 1) (intersection (nth i set1) (nth (+ i j 1) set1) :test test :key key)) r)))))

(defgeneric intersectioon-card (a b &key key test)
 (:documentation "Cardinal de l'intersection des ensembles a et b, ou de tous les éléments de a si b est vide."))

(defmethod intersection-card ((a list) (b list) &key (key #'identity) (test #'equalp))
  (length (intersection (remove-duplicates a :key key :test test)
                        (remove-duplicates b :key key :test test)
                        :key key :test test)))

(defmethod intersection-card ((a list) (b null) &key (key #'identity) (test #'equalp))
  (let ((x (pop a)))
    (loop until (not a)
          do (setf x (intersection (remove-duplicates x :key key :test test)
                                   (remove-duplicates (pop a) :key key :test test)
                                   :key key :test test)))
    (length x)))

(defgeneric jaccard-dist (a b &key test key)
  (:documentation "Jaccard distance.
If a is null, Jaccard distance of both lists of b ;
if b null, as usual in fv-morphologie, matrix of Jaccard distances of lists of a."))

(defmethod jaccard-dist ((a list) (b list) &key (test #'equalp) (key #'identity))
  (let ((o (length (remove-duplicates (append a b) :test #'equalp))))
    (float (/ (- o (intersection-card a b :test test :key key))
              o))))

(defmethod jaccard-dist ((a list) (b null) &key (test #'equalp) (key #'identity))
  (declare (ignore b))
  (let (r (l (length a)))
    (dotimes (i l (nreverse r))
      (dotimes (j (- l i 1))
        (push (list i (+ i j 1) (jaccard-dist (nth i a) (nth (+ i j 1) a)
                                              :key key :test test)) r)))))

(defmethod jaccard-dist ((a null) (b list) &key (test #'equalp) (key #'identity))
  (declare (ignore a))
  (float (/ (intersection-card b nil :key key :test test)
            (length (apply #' append b)))))

;(jaccard-dist '((a b c d e f) (a b c 1 2 3) (b x y)) nil)
;(jaccard-dist 'nil '((a b c d e f) (a b c 1 2 3) (b x y)))
;(jaccard-dist '(1 0 1 0 0 0 0) '(1 0 0 1 0 1 1))

(defgeneric binary-jaccard-dist (a b &key key test)
  (:documentation "Jaccard distance for binary ensembles."))

(defmethod binary-jaccard-dist ((a list) (b list) &key (key #'identity) (test nil))
  (assert (= (length a) (length b)))
  (if test
      (let* ((e (mapcar #'(lambda (x y)
                           (list (funcall test (funcall key x)) (funcall test (funcall key y))))
                       a b))
             (tt (length (remove-if-not #'(lambda (x) (equalp x '(t t))) e)))
             (nn (length (remove-if-not #'(lambda (x) (equalp x '(nil nil))) e)))
             (tn (length (remove-if-not #'(lambda (x) (equalp x '(t nil))) e)))
             (nt (length (remove-if-not #'(lambda (x) (equalp x '(nil t))) e))))
        (float (/ (+ nt tn) (+ nt tn tt))))
    (let* ((o (append a b))
           (bins (remove-duplicates (mapcar key o)))
           (b1 (length (remove (car bins) o :key key)))
           (b2 (length (remove (cadr bins) o :key key)))
           (min (if (< b2 b1) (car bins) (cadr bins))))
      (if (not (= 2 (length bins)))
          (message "binary-jaccard-dist requires binary identities, aborting...")
        (binary-jaccard-dist a b :key key :test #'(lambda (x) (equalp x min)))))))

(defmethod binary-jaccard-dist ((a list) (b null) &key (key #'identity) (test nil))
  (let (r (l (length a)))
    (dotimes (i l (nreverse r))
      (dotimes (j (- l i 1))
        (push (list i (+ i j 1) (binary-jaccard-dist (nth i a) (nth (+ i j 1) a)
                                              :key key :test test)) r)))))

;(binary-jaccard-dist '(1 0 1 0 0 0 0) '(1 0 0 1 0 1 1))
;(binary-jaccard-dist '((a 1) (b 0) (c 1) (d 0) (e 0) (f 0) (g 0)) '((a 1) (b 0) (c 0) (d 1) (e 0) (f 1) (g 1)) :key #'cadr)
;(binary-jaccard-dist '(1 0 1 0 0 0 0) '(1 0 0 1 0 1 1) :test #'(lambda (x) (= 1 x)))
;(binary-jaccard-dist '(1 0 1 0 0 0 0) '(1 0 0 1 0 1 1) :test #'(lambda (x) (= 0 x)))

(defun sorensen-similarity (a b &key (test #'equalp) (key #'identity))
  (let ((ca (length (remove-duplicates a :key key :test test)))
        (cb (length (remove-duplicates b :key key :test test))))
    (float (/ (* 2 (intersection-card a b :test test :key key))
              (+ ca cb)))))

(defun mountford-similarity (a b &key (key #'identity) (test #'equalp))
  (let ((ca (length (remove-duplicates a :key key :test test)))
        (cb (length (remove-duplicates b :key key :test test)))
        (j (intersection-card a b :test test :key key)))
    (float (/ (* 2 j) (- (* 2 ca cb) (* (+ ca cb) j))))))

;(mountford-similarity '(1 0 1 0 0 0 0 2 3 4) '(1 0 0 1 0 1 1))

(defgeneric match (a b))

(defmethod match ((a symbol) (b symbol)) (if (equalp a b) 1 0))

(defmethod match ((a number) (b number)) (if (= a b) 1 0))

(defmethod match ((a symbol) (b list))
  (mapcar #'(lambda (x) (match a x)) b))

(defmethod match ((a list) (b symbol))
  (mapcar #'(lambda (x) (match x b)) a))

(defmethod match ((a list) (b list))
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (match x y)) b)) a))

(defun ref-position (list &optional (test #'equalp))
  (mapcar #'(lambda (e) (position e list :test test)) list))

(defgeneric struct-dist (a b &optional w-occ w-rep test)
  (:documentation   "Resemblance value (0 to 1) between sequences a and b according to their interanl strcutures.
w-occ: weigth for 'occurence structure';
w-rep: weigth for 'repetition' structure."))

(defmethod struct-dist ((a list) (b list) &optional (w-occ 1) (w-rep 1) (test #'equalp))
  (let* ((ma (match a a))
	 (mb (match b b))
	 (occ-a (mapcar #'(lambda (x) (apply #'+ x)) ma))
	 (occ-b (mapcar #'(lambda (x) (apply #'+ x)) mb))
	 (ref-a (ref-position a))
	 (ref-b (ref-position b)))
    (multi-edit-dist (mapcar #'(lambda (x y) (list x y)) occ-a ref-a)
                     (mapcar #'(lambda (x y) (list x y)) occ-b ref-b)
                     (list w-occ w-rep) :inex 0 :test test)))

(defmethod struct-dist ((a list) (b null) &optional (w-occ 1.) (w-rep 1.) (test #'equalp))
  (let ((r 'nil))
    (dotimes (i (length a) (reverse r))
      (dotimes (j (- (length a) (1+ i)))
	(push (list i (+ i j 1)
		    (struct-dist (nth i a) (nth (+ i j 1) a) w-occ w-rep test))
	      r)))))

;;; CONTRAST ANALYSIS - ANALYSE CONTRASTIVE (structure1 in previous version of morphologie)
; rewritten and optimized, thank's to lispme :)

(defun allpos (elt seq &optional (test #'equalp))
  "All positions of elt in seq, using identity  definied in optional argument test"
  (loop for e in seq for i from 0
        when (funcall test elt e)
        collect i))

(defgeneric all-pos (seq elt &optional test cons)
  (:documentation ""))

(defmethod all-pos ((seq t) (elt number) &optional (test #'equalp) (cons T))
  (when (not test) (setf test #'equalp))
  (let ((pos (allpos elt seq test)))
    (if cons pos
      (remove-duplicates pos :test #'(lambda (x y) (= (- x y) 1)) :from-end t))))

(defmethod all-pos ((seq t) (elt symbol) &optional (test #'equalp) (cons T))
  (when (not test) (setf test #'equalp))
  (let ((pos (allpos elt seq test)))
    (if cons pos
      (remove-duplicates pos :test #'(lambda (x y) (= (- x y) 1)) :from-end t))))

(defmethod all-pos ((seq t) (elt list) &optional (test #'equalp) (cons T))
  (sort (remove-duplicates (apply #'append (loop for e in elt collect (all-pos seq e test cons))) :test #'=) '<))

;(all-pos '(0 1 0 0 1 2 1 1 0 1 2) 0 nil nil)
;(all-pos '(0 1 0 0 1 2 1 1 0 1 2) '(0 1) nil nil)

(defun apos (seq &optional (test #'equalp))
  (let* ((e (list (list (pop seq) 0))) (tmp (caar e)) (ki nil) (i 1))
    (loop while seq
       do (setf ki (pop seq))
       (when (not (funcall test ki tmp))
         (let ((m (assoc ki e :test test)))
           (if m
               (setf (cdr (nth (position m e) e)) (append (cdr m) (list i)))
             (setf e (cons (list ki i) e)))))
       (setf tmp ki)
       (incf i))
    (reverse e)))

;(apos '(1 1 2 1 3 1 4 1 2 1 3 1 4) #'=)
;(apos '(a a b b a a a c c a d a e f g h a i a j a k l m n o) #'equal)

(defun segpos (seq pos &optional (rem-loc-rep t) (test #'equalp))
  "The segments of seq starting at the positions pos; with rem-loc-rep, without their local
repetitions (consecutive elements identical according to test)."
  (let ((r (list)))
    (dotimes (i (length pos) (reverse r))
      (if rem-loc-rep
          (setf r (cons (rem-local-rep (subseq seq (nth i pos) (nth (+ i 1) pos)) test) r))
        (setf r (cons (subseq seq (nth i pos) (nth (+ i 1) pos)) r))))))

;(segpos '(a a b a a c a d a b a c a d) (cdar (apos '(a a b a a c a d a b a c a d) #'eq)) t)

(defun segs (seq rem &optional (test #'equalp))
  "segmente la sequence seq pour chaque element different de seq,
selon le critere d'identite des elements defini par la fontion test."
  (let ((r (list)) (p (apos seq test)))
    (do ((i 0 (+ i 1)))	((= i (length p)) (sort r '> :key #'length))
      (setf r (cons (segpos seq (cdr (nth i p) ) rem test) r)))))

;(segs '(a b a a c a d a e f g h a i a j a k l m n o) t)
;(segs '(a b a c a d a e f g h a i a j a k l m n o)
;      (lambda (x y) (< (abs (- (char-code (char (string x) 0)) (char-code (char (string y) 0)))) 3)))

(defun c-seg (seq &optional (rem t) (test #'equalp))
  (mapcar (lambda (x) (append (list (cdr x)) (segpos seq (cdr x) rem test))) (apos seq test)))

;(c-seg '(a b a a c a b a d a e f g h a i a j a k l m n o))

(defun resemble-p  (a b thresh &key (change 1) (insert 1) (delete 1)  (inex 0) (test #'equalp))
  "test based on resemblance usind editing distance."
  (when (not thresh) (setf thresh 0))
  (<= (edit-dist a b change insert delete inex 'T test) thresh))

(defun l-resp (l thresh inex test)
 (let ((r (list)))
   (dotimes (i (length l) (reverse (cdr r)))
     (let ((tmp (list)))
       (do ((j 1 (+ j 1))) ((= j (- (length l) i)))
	 (push (resemble-p (nth i l) (nth (+ i j) l) thresh :inex inex :test test) tmp))  ;;;
       (push (reverse tmp) r) ))))

;(l-resp (car (segs '(a b a c a a b a d a e f g h a b a j a k l m n o) t)) 0 1 #'equalp)

(defun nseg (lseg &optional (thresh 0) (inex 0) (test #'equalp))
  (let ((r (make-list (length lseg)))
	(tmp (l-resp lseg thresh inex test)))
    (dotimes (i (length tmp))
      (when (not (nth i r))
	(let ((k (allpos 't  (cons 't (nth i tmp)))))
	  (when k (mapcar (lambda (x) (setf (nth (+ x i) r) i)) k)))))
    (when (and (null (car (last r))) (car r))
	(setf (nth (1- (length r)) r) (1+ (car (sort (butlast r) '>))))) r))

;(nseg (cdar (c-seg '(a b a c a a a b b a d a e f g h a b a c a b b a k a m n o))) )
;(nseg (cdar (c-seg '(a b a c a d a e f g h a b a c a k a m n o))) .5)

(defun renum (seq &optional (test #'equalp) (start 0))
  "Renums items in seq  according to the position of its first occurences - starts from 1."
  (let (r vu)
   (loop while seq
	do (let* ((elt (pop seq))
		  (o (assoc elt vu :test test)))
	     (if o
		 (push (cadr o) r)
		 (progn
		   (push (list elt (incf start)) vu)
		   (push start r)))))
   (reverse r)))

(defgeneric mark-strct (seq out &optional tolerence rem-loc-rep test)
  (:documentation "Constrative analysis of a sequence <seq>.
Output is sorted by structures's length.
<out> argument specifie output mode:
 :raw for each mark, list of the resulting structure, then list of segments with their positions in seq;
 :struct (default) list of structures.
Optional arguments are:
tolerance: maximum dissemblance threshold (0.0 to 1.0);
rem-loc-dup: t (default) for removing local repetitions in seq, or nil to keep seq as it is;
test: condition of identity for marks, for instance (lambda (x y) (< x y)) (default: #'equalp."))

(defmethod mark-strct ((seq list) (out symbol)  &optional (tolerance 0) (rem-loc-rep t) (test #'equalp))
  (when (not out) (setf out :struct))
  (unless (member out '(:struct :pos :raw))
    (error "mark-structure: unknown output ~S, use :struct, :pos or :raw." out))
  (let ((allsegs (c-seg seq rem-loc-rep test)) (r (list)))
    (dolist (a allsegs)
      (when (> (length a) 1)
	(let ((strct (renum (nseg (cdr a) tolerance 0 test) #'= -1)) ;; may inex value to be controled ?
	      tmp)
          (if (eq :pos out)
              (push (list strct a) r)
            (progn
              (dotimes (i (length (car a)))
                (let ((k (assoc (nth i (cdr a)) tmp :test #'equalp)))
                  (if k
                      (rplacd k (append (cdr k) (list (nth i (car a)))))
                    (push (list (nth i (cdr a)) (nth i (car a))) tmp))) )
              (push (list strct (reverse tmp)) r))))))
    (setf r (remove-if (lambda (x) (< (length x) 2)) r :key #'car))
    (case out
      (:raw (sort r '> :key (lambda (x) (length (car x)))))
      (:struct (sort (mapcar #'car r) '> :key #'length))
      (:pos  (sort r '> :key (lambda (x) (length (car x))))))))

;(mark-strct '(a b a a c a d a e f g h a a b b a c a k a m n o) :pos 0 t)
;(mark-strct '(a b a a c a d a e f g h a a b b a c a k a m n o) :raw 0 t)
;(mark-strct '(a b a c a d a e f g h a b a c a k a m n o) :struct)
;(mark-strct '(a b a c a d a e f g h a b a c a k a m n o) :raw .5)
;(mark-strct '(1 2 1 3 1 4 1 5 6 7 8 1 2 1 3 1 9 1 10 11 12) :raw .5 t (lambda (x y) (< (- y x) 2)))

;;; mark analysis redone again - analyse contrastive autrement
;;; (as a state automaton : a machine mark run A or non-A if A or non-A...)
;;; with A and non-A changing differently state of machine mark...
;; faster... (integre remove-loc-rep !)
;; structure is (segment positions)
;; test : how to test if elt of seq is a mark or not (lambda (a) ...)
;; ident : how to test if elt of seq is same as previous (lambda (x y) ...)
;; if : what to do if mark
;; ifnot : what to do if not mark

(defun doifnot (e c m)
  (declare (ignore c))
  (if (null m) m
      (progn (setf (caar m) (append (caar m) (list e))) (values m))))

(defun doif (e c m)
  (cons (list (list e) (list c)) m))

(defun not-forced (x y) (declare (ignore x y)) nil)

(defun mark (seq test ident if ifnot &optional (c 0) (m nil)) ; may be optimized...
  (if (car seq)
      (let ((e (pop seq)))
	(if m
	    (if (funcall test e) ; if e from seq is a mark...
		(if (funcall ident e (car (last (caar m)))) ; remove local duplicates
		    (mark seq test ident if ifnot (1+ c) m)
		    (mark seq test ident if ifnot (1+ c) (funcall if e c m)))
		(if (funcall ident e (car (last (caar m))))
		    (mark seq test ident if ifnot (1+ c) m)
		    (mark seq test ident if ifnot (1+ c) (funcall ifnot e c m))))
	    (if (funcall test e)
		(mark seq test ident if ifnot (1+ c) (list (list (list e) (list c))))
		(mark seq test ident if ifnot (1+ c) m))))
      (nreverse m)))

(defun posgroup (ldl test r)
  (if (car ldl)
      (let* ((p (pop ldl))
	     (i (position (car p) r :key #'car :test test)))
	(if i
	    (progn (rplacd (nth i r) (list (append (cadr (nth i r)) (cadr p))))
		   (posgroup ldl test r))
	    (posgroup ldl test (cons p r))))
      (sort (nreverse r) '> :key (lambda (x) (length (cadr x))))))

#|
(posgroup (mark '(a b a a a b b a c a a d e f a a b a a c c c a k l l m)
		(lambda (x) (equalp x 'b)) #'equalp #'doif #'doifnot) #'intersection nil)
(position '(e f) '(((a b a) (0 9)) ((d e f) (1))) :key #'car :test #'intersection)
|#

(defun mark-r (seq mark test ident grouptest) ;; fusionner  test+ident ????
  (if mark
      (posgroup (eval `(mark ',seq (lambda (x) (funcall ,test x ',mark)) ,ident #'doif #'doifnot))
		grouptest  nil)
      (mapcar (lambda (x) (mark-r seq x test ident grouptest)) (remove-duplicates seq :test test))))

(defgeneric mark-pos (seq mark &optional test ident) ;;;;;;;test2! > fusionner test+ident = test1?
  (:documentation "For each 'mark', returns all subseq starting with the mark consed to its positions in seq.
If no mark specified, returns subseqs and positions for mark.
Optional arguments:
test: test defining a mark - for instance: #'equalp ;
ident: test to compare resulting subsequences, for instance: #'equalp."))

(defmethod mark-pos ((seq list) (mark t) &optional (test #'equalp) (ident #'equalp))
  (mark-r seq mark test test ident))

(defmethod mark-pos ((seq list) (mark null) &optional (test #'equalp) (ident #'equalp))
  (sort (mark-r seq mark test test ident) '> :key #'length))

; (mark-pos '(a b a a a b b a c a a d e f a a b a a c c c a k l l m) 'b #'equalp)

;;;
;; contrasts-all-lev from Paolo Aralla rewritten

(defun renum-mat (seq &optional (test #'equalp) r)
  (if seq
      (renum-mat (cdr seq) test (cons (renum seq test) r))
      (butlast (reverse r))))

; (renum-mat  '(a b a c a d a e f g h a b a c a k a m n o))

(defun new-old-analysis (seq &optional (test #'equalp))
  "'new-old' Paolo Aralla algorithm inspired by contrast/mark analysis, rewritten."
  (let* ((seq+ (cons '*xstart* (append seq '(*xstop*))))
	 (d (mapcar #'dx (renum-mat seq+ test)))
	 (w (mapcar (lambda (x) (apply #'+ x)) (renum-mat seq+ test)))
	 (c (mapcar (lambda (x y) (mapcar (lambda (z) (* x z)) y)) w d)))
    (butlast (reverse (mapcar (lambda (x) (apply #'+ x)) (ldlrot (mapcar #'reverse c)))))))

;(new-old-analysis '(a b c a d e a))

(defgeneric mark-dynamic (seq &optional test)
  (:documentation "Kind of inner dynamic of the distribution of the different symbols (or marks) in the sequence <seq>,
computed by a kind of integration of contrast/mark analysis, novelty of marks and their echoing in the sequence.
Code is rewritten from Paolo Aralla so called 'energy profil' - may be tuned eventually.
Cf. also Baboni-Schilingi, Giacomo Platini."))

(defmethod mark-dynamic ((seq list) &optional (test #'equalp ))
  (mapcar #'abs (dx (mapcar #'abs (cons 0 (new-old-analysis seq test))))))

; (mark-dynamic '(a b a c a d a e f g h a b a c a k a m n o))

(defun cutifback (seq &key (test #'equalp))
"Yet another trivial cut following more or less an established general idea of
mark/contrast-analysis : cut when an old elt is back according to keyword test for sameless or 'identity'
(cf. Fred)"
  (let ((r '()) (old '()) tmp)
   (loop while seq
	do (let ((elt (pop seq)))
	     (if (member elt old :test test)
		 (progn
		   (push (reverse tmp) r)
		   (setf tmp (list elt)))
		 (progn
		   (push elt tmp)
		   (push elt old)))))
   (reverse r)))

;(cutifback '(a b a c a d a e f g h a b a c a k a m n o))

(defun subpos-p (posa posb)
  "Is posa sub-positions of posb ?"
  (cond ((and (integerp posa) (integerp posb)) (= posa posb))
	((and (listp posa) (listp posb)) (and (>= (car posa) (car posb))
					(<= (cadr posa) (cadr posb))))
	(t nil)))

;(subpos-p '(1 4) '(0 4))
;(subpos-p '(0 4) '(1 4))
;(subpos-p '(0 4) '(3 8))

(defun l-subpos-p (a b)
  (and (= (length a) (length b))
       (every #'subpos-p a b)))

;(l-subpos-p '(1 4) '(0 4))

;;; PATTERN STRUCTURE is old Structure 2 totally rewritten

(defgeneric find-pos (motif seq &optional diss l-var distance-fct)
  (:documentation "Yet another find pattern in a sequence with resemblance (default) criteria.
Returns the positions of sequence <ptrn> into sequence <seq> with some tolerance in comparing to be adjusted by optional arguments.
<motif> : motif to search;
<seq> : sequence of symbols.
Optional argument: distance function in lambda form, for instance : (lambda (a b) (edit-dist a b))."))

(defun edit-dist-bounded (a b start lb bound &optional (test #'equalp))
  "The edit distance (costs 1) between the vector a and the lb elements of the vector b from
position start, if it is not more than the integer bound, else nil. The computation stops as
soon as the distance is certainly more than bound: the minimum of a row of the matrix of costs
never decreases in the next rows."
  (declare (simple-vector a b) (fixnum start lb bound) (function test))
  (let ((la (length a)))
    (declare (fixnum la))
    (when (<= (abs (- la lb)) bound)
      (let ((prev (make-array (1+ la) :element-type 'fixnum))
            (row (make-array (1+ la) :element-type 'fixnum)))
        (declare (type (simple-array fixnum (*)) prev row))
        (dotimes (i (1+ la)) (setf (aref prev i) i))
        (loop for j fixnum from 1 to lb
              do (setf (aref row 0) j)
                 (let ((m j) (bj (svref b (+ start j -1))))
                   (declare (fixnum m))
                   (loop for i fixnum from 1 to la
                         do (let ((d (min (+ (aref prev (1- i))
                                             (if (funcall test (svref a (1- i)) bj) 0 1))
                                          (1+ (aref prev i))
                                          (1+ (aref row (1- i))))))
                              (declare (fixnum d))
                              (setf (aref row i) d)
                              (when (< d m) (setf m d))))
                   (when (> m bound) (return-from edit-dist-bounded nil)))
                 (rotatef prev row))
        (let ((d (aref prev la)))
          (when (<= d bound) d))))))

(defun find-pos-in (motif v start diss l-var distance-fct &optional (test #'equalp))
  "Positions (first last) of motif in the vector v, from position start (see find-pos).
Without distance-fct, the normalized edit distance is used, test being the identity of elements;
with diss 0 too, the windows are compared element by element with test, which gives the same
result without computing distances."
  (let* ((m (coerce motif 'simple-vector))
         (v (coerce v 'simple-vector))
         (l (length m))
         (lseq (length v))
         (l+ (floor (* l l-var)))   ; for length tolerance
         (exact (and (null distance-fct) (zerop diss)))
         ;; with a margin for the rounding of floats
         (rdiss (+ (rational diss) 1/1000000))
         pos)
    (flet ((match-p (i j)
             (if exact
                 (and (= j l)
                      (loop for k below l always (funcall test (aref m k) (aref v (+ i k)))))
                 (if distance-fct
                     (>= diss (funcall distance-fct motif (coerce (subseq v i (+ i j)) 'list)))
                     ;; the normalized edit distance, computed only while it may be <= diss;
                     ;; same decision as before
                     (let* ((maxlen (max l j))
                            (d (edit-dist-bounded m v i j (floor (* rdiss maxlen)) test)))
                       (and d (>= diss (float (/ d maxlen)))))))))
      (loop for i from start to (- lseq l+ 1)
            do (loop for j from (max 1 (- l l+)) to (+ l l+)
                     while (<= (+ i j) lseq)
                     when (match-p i j)
                       do (push (list i (+ i j -1)) pos)
                          (return))))
    (reverse (remove-if #'(lambda (x)
                            (some #'(lambda (a) (subpos-p x a))
                                  (remove x pos :test #'equalp)))
                        pos))))

(defmethod find-pos ((motif list) (seq list) &optional (diss 0) (l-var 0) (distance-fct nil))
  (find-pos-in motif (coerce seq 'vector) 0 diss l-var distance-fct))

; (time (find-pos '(a b c) '(t x a  b c y  t a e  b c k  r a b  c)))
; (time (find-pos  '(a b c) '(t x a b c y t a e b c k r a b c) .25 .34))
; (time (find-pos  '(a b c) '(t x a b c y t a e b c k r a b c) .25 .2 1 0 1 0))

(defun subseq-p (a b &optional (test #'equalp))
  (let ((la (length a))
        (lb (length b))
        (pos 0)
        x)
    (if (or (not a) (not b)) ; trivial test
	nil
      (if (< la lb)
          (loop for i from 0 to (- lb la)
                until (setf x (funcall test a (subseq b i (+ i la))))
                do (setf pos i)
                finally (incf pos))
        (if (> la lb)
            (loop for i from 0 to (- la lb)
                  until (setf x (funcall test b (subseq a i (+ i lb))))
                  do (setf pos i)
                  finally (incf pos))
          (if (equalp a b)  ; another trivial test
              (setf x t)
            (setf x (funcall test a b))))))
    (values (if x pos x))))

;(subseq-p '(a b c) '(x y a b c z))
;(subseq-p '(a b c) '(x y d a b e c z) #'(lambda (a b) (<= (edit-dist a b 1 1 1 0 T) .34)))
;(subseq-p '(a b c) '(x y d a b e c z) #'(lambda (a b) (<= (edit-dist a b 1 1 1 0 T) .25)))
;(subseq-p '(a b c) '(x y a b z))
;(subseq-p '(a b c) '(x y a c d z) #'(lambda (a b) (<= (- (char-code (elt (format nil "~S" a) 0)) (char-code (elt (format nil "~S" b) 0))) 1)))

(defun whenlooploopwhenwhen (seq &optional (diss 0) (l-var 0) (n nil) (distance-fct nil) (test #'equalp))
  (let ((r (list))
	(n- 2)
	(n+ nil)
	(l (length seq))
	(v (coerce seq 'vector)))
    ;; distance-fct nil: the normalized edit distance (see find-pos-in)
    (when (not n) (setf n (list 3 (floor (/ l 2)))))
    (if (numberp n)
        (setf n+ n)
      (setf n- (car n)
            n+ (cadr n)))
    (loop for i from 0 to (- l (max 2 n-)) 
       do (loop for j from n- to (if n+ (min n+ (- l i)) (- l i))
	     do (let (f (candidat (subseq seq i (+ i j))))
		  ;; skip a candidate identical to an earlier motif, or which is one of its repetitions
		  (when (not (or (assoc candidat r :test #'equalp)
				 (let ((range (list i (+ i j -1))))
				   (some #'(lambda (x) (member range (cdr x) :test #'equal)) r))))
		    (setf f (find-pos-in candidat v (+ i j) diss l-var distance-fct test)))
		  (when f (push (append (list candidat) (cons (list i (+ i j -1)) f)) r)))))
    (remove-if #'(lambda (x)
		   (some #'(lambda (a) (l-subpos-p (cdr x) (cdr a)))
			 (remove x r :test #'equalp)))
	       r)))

#|
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
			     0 0 '(3 5)))
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
			     0.25 0.25 '(3 6)))
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
            ;;                 0 1 2  3 4 5  6      9     12     15     18     21     24     27     30
			     0.25 0.25 '(3 8))
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
			      .0 .0 nil)
(print (whenlooploopwhenwhen '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r)
			     0 .4 nil)
|#

;; next function does also an brut-force run on the seqs which may be not well adapted to huge lists
;; for huge lists of symbols, I rather like to do the same but using an random search to be tuned - kind of sondage or windowing -
;; in the list, following more flexible or fuzzy/lazy rules for probabiltistic retrieval - may be use of a dict ?

(defgeneric find-self (seq out &optional diss l-var n distance-fct test)
  (:documentation "List all patterns in seq found using ptrn-find."))

(defmethod find-self ((seq list) (out symbol) &optional (diss 0) (l-var 0) (n nil) (distance-fct nil) (test #'equalp))
  ;; distance-fct nil: the normalized edit distance with test (see find-pos-in)
  (when (null diss) (setf diss 0))
  (when (null l-var) (setf l-var 0))
  (when (null out) (setf out ':length))
  (case out
    (:length
     (sort (whenlooploopwhenwhen seq diss l-var n distance-fct test) '> :key (lambda (x) (length (car x)))))
    (:freq
     (sort (whenlooploopwhenwhen seq diss l-var n distance-fct test) '> :key (lambda (x) (length (cdr x)))))
    (t (whenlooploopwhenwhen seq diss l-var n distance-fct test))))

;(print (find-self '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :length))
;(print (find-self '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :freq))

(defun insert-no-overlap (seq i &optional (test #'(lambda (x) (> x 0))))
  "i liste (min max). Les bornes - car et cadr - ne se recouvrent pas. Test par defaut : (lambda (x) (> x 0)) "
  (let ((diffmin (mapcar #'(lambda (x) (- (car i) (cadr x))) seq))
        (diffmax (mapcar #'(lambda (x) (- (car x) (cadr i))) seq))
        a b)
    (when (not test) (setf test #'(lambda (x) (> x 0))))
    (if (and (setf a (position-if test diffmin :from-end t))
             (setf b (position-if test diffmax :from-end nil)))
        (append (subseq seq 0 (1+ a)) (list i) (subseq seq b))
      (if a
          (if (= a (1- (length seq)))
              (append seq (list i))
            seq)
        (append (list i) seq)))))

;(insert-range '((0 10) (20 25) (30 32)) '(27 29))
;(insert-range '((0 10) (20 25) (30 32)) '(11 19))
;(insert-range '((0 10) (20 25) (30 32)) '(25 30))
;(insert-range '((0 10) (20 25) (30 32)) '(25 30) #'(lambda (x) (>= x 0)))
;(insert-range '((0 10) (20 25) (30 32)) '(33 39))
;(insert-range '((0 10) (20 25) (30 32)) '(-8 -1))
;(insert-range '((0 10) (20 25) (30 32)) '(0 5))
;(insert-range '((0 10) (20 25) (30 32) (35 39)) '(30 33)) 
;(insert-range '() '(0 9))

(defun insert-all (seq i &optional test)
  "i liste (min max).  Les bornes - car et cadr - peuvent se recouvrir."
  (let ((s (sort (append seq (list i)) '< :key #'car)))
    (sort s '< :key #'cadr)))

;(insert-all '((0 10) (20 25) (30 32) (35 39)) '(31 36))
;'((0 10) (20 25) (30 (31 32) (35 36) 39))

;(cdr '(0 2 (0 4)))
;(cdr '(10 (13 15) 19))

(defun insert-tree (seq i &optional test)
  ""
  (let ((diffmin (mapcar #'(lambda (x) (- (car i) (cadr x))) seq))
        (diffmax (mapcar #'(lambda (x) (- (car x) (cadr i))) seq))
        a b)
    (when (not test) (setf test #'(lambda (x) (> x 0))))
    (if (and (setf a (position-if test diffmin :from-end t))
             (setf b (position-if test diffmax :from-end nil)))
        (append (subseq seq 0 (1+ a)) (list i) (subseq seq b))
      (if a
          (if (= a (1- (length seq)))
              (append seq (list i))
            seq)
        (append (list i) seq)))))

; (((T X A B C) (0 4) (16 20))
; ((B C K R) (9 12) (29 32))
; ((Y T A E) (5 8) (22 25))
; ((T X A) (0 2) (13 15) (16 18)))

(defgeneric rep-strct (seq &key diss l-var n criteria distance-fct test)
  (:documentation "Possibles structures of seq based of automatically found patterns (plist)."))

(defmethod rep-strct ((seq t) &key (diss 0) (l-var 0) (n nil) (criteria :length) distance-fct (test #'equalp))
  ;; without distance-fct, the normalized edit distance with test (with diss 0: element by element)
  (let* ((x (whenlooploopwhenwhen seq diss l-var n distance-fct test))
         (ptrns
          (case criteria
            (:length
             (remove-duplicates (sort x '> :key #'(lambda (x) (length (car x))))
                                :from-end t
                                :test #'(lambda (a b) (and (subseq-p (car a) (car b)) (= (length (cdr a)) (length (cdr b)))))))
            (:freq
             (remove-duplicates (sort x '> :key #'(lambda (x) (length (cdr x))))
                                :from-end nil
                                :test #'(lambda (a b) (and (subseq-p (car a) (car b)) (= (length (cdr a)) (length (cdr b)))))))
            (t (remove-duplicates x
                                  :from-end t
                                  :test #'(lambda (a b) (and (subseq-p (car a) (car b)) (= (length (cdr a)) (length (cdr b)))))))))
         (pos (sort (remove-duplicates (apply #'append (mapcar #'(lambda (x) (mapcar #'car (cdr x))) ptrns))) '<))
         (ptrn-list nil)
         (st nil))
    (mapcar #'(lambda (x) (setf (cdr x) (mapcar #'car (cdr x)))) ptrns)
    (dotimes (k (length ptrns))
      (push (list (car (nth k ptrns)) k) ptrn-list))
    (dolist (p pos)
      (push (apply #'append (list (list p)
                                  (mapcar #'(lambda (x) (cadr (assoc (car x) ptrn-list :test #'equalp)))
                                          (remove-if-not #'(lambda (x) (member p (cdr x))) ptrns))))
            st))
    (list (reverse st) (mapcar #'car (sort ptrn-list '< :key #'cadr)))))
    
#|
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria :length))
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria :freq)
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria nil)
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria :freq
                  :diss 0.25 :l-var 0.3 :n '(2 8)))
(print (rep-strct '(t x a  b c y  t a e  b c k  r t x  a t x  a b c  i y t  a e c  e i b  c k r) :criteria nil
                  :diss 0.25 :l-var 0.3 :n '(2 8)))
|#

(defun permut-p (l1 l2)
  (not (or (member 'nil (mapcar #'(lambda (x) (member x l1)) l2))
	   (member 'nil (mapcar #'(lambda (x) (member x l2)) l1)))))


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
  (let* ((v (all-vertices l-graph))
         (m (make-list (length v))))
    (dotimes (i (length m))
      (setf (nth i m) (make-list (length v) :initial-element 0)))
    (dolist (g l-graph m)
      (let ((i (position (car g) v :test #'eq))
            (j (position (cadr g) v :test #'eq))
            (d (caddr g)))
        (setf (nth j (nth i m)) d
              (nth i (nth j m)) d)))))

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

;;; CLASSIFICATION

(defun pos (item list)
  (loop for n from 0 to (1- (length list))
     when (= (nth n list) item) collect n))

(defun normalize-class (classes &optional (test #'equalp))
  "Reordonne les classes de nueesdynamiques, l'ordre de la classe etant le numero de sa premiere occurence dans la liste des classes."
  (let ((set nil) (r nil) (marker -1))
    (when (stringp classes) (setf classes (str->symb classes)))
    ;; set: the classes already seen, as (class . number), most recent first
    (dolist (c classes (nreverse r))
      (let ((seen (assoc c set :test test)))
        (if seen
            (push (cdr seen) r)
            (progn (push (cons c (incf marker)) set)
                   (push marker r)))))))

(defun norm-class (classes)
  "Reordonne les classes de class-1.
L'ordre de la classe etant le numero de sa premiere occurence dans la liste des classes.
Se connecte typiquement apres class-1 ou meta-class1.
IN : string or list (of symbols or lists or strings)."
  (if (or (stringp classes) (not (listp (car classes))))
      (normalize-class classes)
      (mapcar #'(lambda (x) (normalize-class x)) classes)))

(defun random-dot (data)
  "A random point in the space defined by data"
  (loop for j from 0 to (length (car data))
     collect  (let* ((min (apply #'min (mapcar #'(lambda (x) (nth j x)) data)))
		     (max (apply #'max (mapcar #'(lambda (x) (nth j x)) data)))
		     (range (- max min)))
		(if (minusp max)
                        (float (+ min (* -1 (random (float range)))))
			(float (+ min (random  (float range))))))))

;(random-dot '((0 0) (1 0) (0 -1) (2 -.1) (.1 -2)))

(defun isobarycentre (points)
  (let ((c (make-list (length (car points)) :initial-element 0))
        (l (length  points)))
    (loop until (null points)
       do (loop for p in (pop points) for i from 0 
	     do (incf (nth i c) p)))
    (mapcar #'(lambda (x) (float (/ x l))) c)))

;;; The 'Dynamic Clouds' algorithm (nuees dynamiques) starts from n distinct
;;; points of data chosen at random as centres. A class which becomes empty
;;; receives the point farthest from its centre, so that n classes are found
;;; as long as data has at least n distinct points.

(defgeneric nuees (data n &optional dist)
  (:documentation   "'Nuees dynamiques' algorithm :
Classify data into n classes (n integer), or around the initial centres n (n list of points).
Returns: classes of data, respective centers coordinates and iterations.
There are fewer than n classes only if data has fewer than n distinct points."))

(defun random-centres (data n)
  "n distinct points of data chosen at random (all of them if there are fewer)."
  (let ((points (remove-duplicates data :test #'equalp)))
    (loop repeat (min n (length points))
          collect (let ((p (nth (random (length points)) points)))
                    (setf points (remove p points :test #'eq))
                    (copy-list p)))))

(defmethod nuees ((data list) (n integer) &optional (dist #'dist-euclid))
  (nuees data (random-centres data n) dist))

;(nuees '((0 0) (.1 -.1) (.1 0)  (1 1) (1.1 1.1)  (.9 1) (1.9 .5)  (-2 -2) (-2.1 -1.5)) 3)
;(nuees '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 3)

(defun fill-empty-classes (r data centres dist)
  "Gives each empty class of r the point farthest from its centre, taken from a class with several points."
  (dotimes (k (length centres) r)
    (unless (member k r)
      (let ((far nil) (dmax -1))
        (loop for i from 0 for c in r for p in data
              do (when (> (count c r) 1)
                   (let ((d (funcall dist (nth c centres) p)))
                     (when (> d dmax) (setf dmax d far i)))))
        (when far
          (setf (nth far r) k
                (nth k centres) (copy-list (nth far data))))))))

(defmethod nuees ((data list) (n list) &optional (dist #'dist-euclid))
  (let* ((l (length data))
         (centres (copy-tree n))
         (r (make-list l))
         (fill (<= (length centres) (length (remove-duplicates data :test #'equalp))))
         (iter 0)
         (nc (make-list (length centres)))
         (z 0)
         tmp nr)
    (when (not dist) (setf dist #'dist-euclid))
    (loop until (or (if tmp (equalp (car tmp) (cadr tmp)) nil)
                    (>= iter 1000))
          do (incf iter)
             (setf tmp (list (copy-tree r) (car tmp)))
             (loop for i from 0 to (1- l) do ; attribution des classes selon centre
               (let ((d (mapcar #'(lambda (c) (when c (funcall dist c (nth i data)))) centres)))
                 (let ((m (apply #'min (remove nil d))))
                   (setf (nth i r) (position-if #'(lambda (x) (and x (= x m))) d)))))
             (when fill (fill-empty-classes r data centres dist))
             (dolist (i (remove-duplicates r))  ; mise a jour des centres de gravite de chaque classe
               (let ((elts (loop for j from 0 to (1- l)
                                 when (= i (nth j r))
                                   collect (nth j data))))
                 (when elts (setf (nth i centres) (isobarycentre elts))))))
    (setf nr (normalize-class r #'=)) ; output
    (loop until (or (not (member nil nc)) (= z l)) do
      (setf (nth (nth z nr) nc) (nth (nth z r) centres))
      (incf z))
    (values nr nc iter)))

(defgeneric dynclouds (data n &optional iter dist)
  (:documentation "Classification of data into N classes using the 'dynamic clouds' algorithm.
Adapted from Diday & all: Elements d'analyse de donnees, Dunod, Paris, 1982.
The first optional argument <iter> fixes the output form:
if nil, runs the algorithm once and returns (classes centres iterations) ;
if <iter> is a list of one number k, runs the algorithm k times and returns each different classification
with its frequency (the frequencies sum to 1), the most frequent first ;
if <iter> is a number k, runs the algorithm k times and returns two values: the most frequent classification and its frequency ;
if t, the same with N * N runs, where N is the number of classes to be found, and returns the most frequent classification.
The second argument <dist> is the distance function (#'dist-euclid by default).
"))

(defmethod dynclouds ((data list) (n t) &optional (iter nil) (dist nil))
  (when (not (member nil (mapcar #'numberp data)))
    (setf data (mat-transpose (list data (make-list (length data) :initial-element 0)))))
  (cond
   ((null iter) (multiple-value-bind (r c i) (nuees data n dist) (list r c i)))
   ((listp iter) (let* ((k (max 1 (car iter)))
                        (r (loop repeat k
                                 collect (values (nuees data n dist))))
                        (p (remove-duplicates r :test #'equalp)))
                   (sort (mapcar #'(lambda (x)
                                     (list x (float (/ (count x r :test #'equalp) k))))
                                 p)
                         '> :key #'cadr)))
   ((integerp iter)
    (let ((r (dynclouds data n (list iter) dist)))
      (values (caar r) (cadar r))))
   (t (caar (dynclouds data n (list (* n n)) dist)))))

;(dynclouds '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 2 nil)
;(dynclouds '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 2 t)
;(dynclouds '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 2 8)
;(dynclouds '((0) (.1) (.11)  (1) (1.1)  (.9) (1.9)  (-2) (-2.1)) 2 '(8))

(defgeneric n-class (data n &optional iter dist)
  (:documentation "Classification of data into N classes using the 'dynamic clouds' algorithm.
Adapted from Diday & all: Elements d'analyse de donnees, Dunod, Paris, 1982.
The first optional argument <iter> fixes the output form:
if nil, runs the algorithm once and returns (classes centres iterations) ;
if <iter> is a list of one number k, runs the algorithm k times and returns each different classification
with its frequency (the frequencies sum to 1), the most frequent first ;
if <iter> is a number k, runs the algorithm k times and returns two values: the most frequent classification and its frequency ;
if t, the same with N * N runs, where N is the number of classes to be found, and returns the most frequent classification.
The second argument <dist> is the distance function (#'dist-euclid by default).
"))

(defmethod n-class ((data list) (n t) &optional (iter nil) (dist nil))
  (if data (dynclouds data n iter dist) nil))

(defgeneric 1d-class (data n &optional fct)
  (:documentation "quantifies data by partitioning dx."))

(defun 1d-cut-centres (sx cuts)
  "Means of the segments of the sorted list sx, cut after each position of cuts."
  (let ((start 0))
    (loop for end in (append (sort (copy-list cuts) '<) (list (1- (length sx))))
          collect (isobarycentre (mapcar #'list (subseq sx start (1+ end))))
          do (setf start (1+ end)))))

(defun 1d-nuees (data centres)
  "Runs the dynamic clouds algorithm on 1D data from the initial centres."
  (multiple-value-bind (r c i)
      (nuees (mat-transpose (list data (make-list (length data) :initial-element 0)))
             (mapcar #'(lambda (x) (list (car x) 0)) centres))
    (list r (mapcar #'car c) i)))

(defmethod 1d-class ((data list) (n null) &optional (fct #'identity))
  (declare (ignore n))
  (let* ((sx (sort (copy-list data) '<))
         (fx (mapcar fct (dx sx)))
         (c2 (nuees (mat-transpose (list fx (make-list (length fx) :initial-element 0))) 2))
         ;; the class of the largest gaps
         (big (nth (position (apply #'max fx) fx) c2)))
    (1d-nuees data (1d-cut-centres sx (allpos big c2)))))

(defmethod 1d-class ((data list) (n integer) &optional (fct #'identity))
  (let* ((sx (sort (copy-list data) '<))
         (fx (mapcar fct (dx sx)))
         (k (min (1- n) (count-if #'plusp fx)))
         ;; positions of the n-1 largest gaps
         (cuts (mapcar #'cdr (subseq (stable-sort (loop for g in fx for i from 0 collect (cons g i))
                                                  '> :key #'car)
                                     0 k))))
    (1d-nuees data (1d-cut-centres sx cuts))))

(defun -reverse (list r)
  "A special working reverse for dotted lists."
    (if (listp list)
	(-reverse (cdr list) (cons (car list) r))
      (cons list r)))

;(-reverse (cons 1 '(2)) nil) !!crash
;(-reverse (cons 1 2) nil)
;(-reverse '(1 2 . 3) nil)
; > flatten

;; groupadjif1 / 2 a optimiser !

(defun groupadjif1 (data test r)
  "Groups adjacent elts in data if test is verified. Elts are atoms."
  (if (null data)
      (reverse (mapcar #'(lambda (x) (if (listp x) (-reverse x nil) x)) r))
    (let ((x (pop data)))
      (groupadjif1 data test
		   (if r
		       (let* ((c (car r)) (y (if (listp c) (car c) c)))
			 (if (funcall test y x)
			     (if (listp c)
				 (cons (cons x c) (cdr r))
				 (cons (cons x c) (cdr r)))
			     (cons x r)))
		       (cons x r))))))

;(groupadjif1 '(1 2 3 3 4 5 6 6 6 6 7 8 9) #'= nil) ;duscutable cf v if2

(defun groupadjif2 (data test r)
  (if (null data)
      (nreverse r) ;(mapcar #'(lambda (x) (apply #'append x)) r))
     (let ((x (pop data)))
      (groupadjif2 data test
                   (if r
                       (let* ((c (car r)) ); (y (if (listp (car c)) (car c) c)))
                         ;(print (list c x r))
                         (if (funcall test c x) ;y
                             (cons (append c x) (cdr r))
                           (cons x r)))
                     (cons x r))))))

;(groupadjif2 '((0 1) (1 8) (2 1) (3 8) (4 8) (5 8) (9 1) (9 1) (9 2) (1 1)) #'(lambda (x y) (= (cadr x) (cadr y))) nil)

(defgeneric adj-group (seq test &optional key)
  (:documentation "Groups adjacent elts in data according to the function test ('eq by default).
A function key specifies the position or value to be tested for each element of seq with optional argument.
If no key (by default), the test apply to the element itself."))

(defmethod adj-group ((seq list) (test function) &optional (key nil))
  (when (not (functionp test)) (setf test #'equalp)
    (message "adj-group: test is not a function, using test #'equalp"))
  (cond ((not (member nil (mapcar #'atom seq)))
         (if (not key)
             (groupadjif1 seq test nil)
           (groupadjif1 seq #'(lambda (x y) (funcall test (funcall key x) (funcall key y))) nil)))
        ((not (member nil (mapcar #'listp seq)))
         (when (not key) (setf key #'(lambda (x) (remove-duplicates x :test #'equalp)))) ; #'identity))
         (groupadjif2 seq #'(lambda (x y) (funcall test (funcall key x) (funcall key y))) nil))) )

(defmethod adj-group ((seq list) (test t) &optional (key nil))
  (adj-group seq #'equalp key))

;(adj-group '(0 1 2 3 3 3 4 3 2 1 1 1 1 2 1) #'=)
;(adj-group '((0 1) (1 8) (2 1) (3 8) (4 8) (5 8) (9 1) (9 1) (9 2) (1 1)) #'(lambda (x y) (= (cadr x) (cadr y))) )
;(adj-group '((0 1) (1 8) (2 1) (3 8) (4 8) (5 8) (9 1) (9 1) (9 2) (1 1)) #'(lambda (x y) (= x y)) #'cadr)

;;;
;;; classification par partitionnement de graphe

(defun sym-isocentre (data)
  "To make simple, the centre of a list of words is the list of words with no duplicates ;)"
  (remove-duplicates (mapcar #'append data) :test #'equalp))

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

(defun s-class (data n &optional (norm nil) (change 1) (insert 1) (delete 1) (inex 1) (mst nil)) ; (dist #'e-dist 1 1 .5)
  "Classification of symbolic data into n classes according to their ressemblance:
the n-1 longest edges of the minimum spanning tree of the data are cut (single linkage).
Returns the class of each datum, numbered by order of first appearance, and the list of classes."
  (let* ((symlist (remove-duplicates data :test #'equalp))
         (grm (cond (mst mst)
                    ((cdr symlist)
                     (minimum-spanning-tree (edit-dist symlist nil change insert delete inex norm)))
                    (t nil)))
         ;; the edges kept: all but the n-1 longest (the first one in the tree if equal)
         (kept (nthcdr (min (max 0 (1- n)) (length grm))
                       (stable-sort (copy-list grm) '> :key #'third)))
         (symclass (tree-components (length symlist) kept))
         (class-of (mapcar #'(lambda (d) (nth (position d symlist :test #'equalp) symclass)) data))
         (renum (normalize-class class-of #'=)))
    (values renum
            (loop for c from 0 to (if renum (apply #'max renum) -1)
                  collect (list c (loop for s in symlist for i from 0
                                        when (= c (nth (position s data :test #'equalp) renum))
                                          collect (list i s)))))))

(defun s-class-with-fixed (data n fixed &optional (fixed-dist nil) (norm nil) (mode t) (change 1) (insert 1) (delete 1) (inex 1) (mst nil))
  (declare (ignore mode))
  (let* ((f-dist (if fixed-dist fixed-dist #'equalp)) ; may be edit-dist change insert inex ?
         (f-length (length fixed))
         (fixed-pos (mat-transpose (list (mapcar #'(lambda (f) (allpos f data f-dist)) fixed) fixed)))
         (data-f (remove-if #'(lambda (x) (member x fixed :test f-dist)) data)) ; :test #'equalp))
         (c-data (s-class data-f n norm change insert delete inex mst)))
    (loop ;for d in data
          for i from 0 to (1- (length data))
          collect (let ((k (assoc i fixed-pos :test #'member)))
		    (if k
			(position (cadr k) fixed :test #'equalp)
			(+ f-length (pop c-data)))))))

;; s-interpolate.lisp
;; interpolate two lists as sequences of symbols using some levenshtein distance algorithm.
;; Fred Voisin, pour Julien Vincenot, 2013.

(defun s2l (string)
 (let ((r nil))
    (dotimes (n (length string) (nreverse r))
      (when (not (eql #\ (elt string n)))
	(push (read-from-string (string (elt string n))) r)))))

(defun m-levenshtein (s1 s2 &key (test #'eql))
 (let* ((width (1+ (length s1)))
	 (height (1+ (length s2)))
	 (d (make-array (list height width)
                       :element-type 'fixnum))
        (s1 (coerce s1 'vector))
        (s2 (coerce s2 'vector)))
   (dotimes (x width)
     (setf (aref d 0 x) x))
   (dotimes (y height)
     (setf (aref d y 0) y))
   (dotimes (x (length s1))
     (dotimes (y (length s2))
       (let ((cost (if (funcall test (aref s1 x) (aref s2 y)) 0 1)))
         (setf (aref d (1+ y) (1+ x))
               (min (1+ (aref d y (1+ x)))
                    (1+ (aref d (1+ y) x))
                    (+ (aref d y x)
                       cost))))))
   (values d)))

(defun ins (a b x y)
  (let ((a1 (subseq a 0 (1+ x)))
        (a2 (subseq a (1+ x))))
    (append a1 (list (nth y b)) a2)))

(defun sup (a b x y)
  (declare (ignore b) (ignore y))
  (append (subseq a 0 x) (subseq a (1+ x))))

(defun sub (a b x y)
  (let ((a1 (subseq a 0 x))
        (a2 (subseq a (1+ x))))
    (append a1 (list (nth y b)) a2)))

(defun s-interpolate (a b &key (test 'eql))
  (let ((m (m-levenshtein b a :test test))  ; matrice
        (p (list (list (length b) (length a)))) ; positions
        (r (list b)) ; resultat
        (o nil)) ; operation
    (loop until (eval `(and .,(mapcar #'zerop (car p))))
          do
          (let* ((x (caar p))
                 (y (cadar p))
                 (np nil)
                 (coutsvoisins (if (zerop x)
                                   (if (zerop y)
                                       (abort)
                                     (list (list x (1- y) (aref m (- y 1) x))))
                                 (if (zerop y)
                                     (list (list (1- x) y (aref m y (- x 1))))
                                   (list (list (1- x) (1- y) (aref m (- y 1) (- x 1)))
                                         (list (1- x) y (aref m y (- x 1)))
                                         (list x (1- y) (aref m (- y 1) x))))))
                 (min (apply #'min (mapcar #'third coutsvoisins))))
            (setf coutsvoisins (remove-if-not #'(lambda (x) (= min (caddr x))) coutsvoisins))
            (if (= 1 (length coutsvoisins))
                (setf np (car coutsvoisins))
              (setf np (nth (random (length coutsvoisins)) coutsvoisins)))
            (setf o (if (= x (car np))
                        (if (= y (cadr np))
                            (abort)
                          (setf o #'ins))
                      (if (= y (cadr np))
                          (setf o #'sup)
                        (setf o #'sub))))
	    (push (funcall o (car r) a (1- x) (1- y)) r)
            (push (butlast np) p)))
    (values r)))

;(s-interpolate '(a m p h o r a) '(p o u l e t))


;;;
;; stats

(defun histogramme (data &key (test #'equalp) (thes nil))
  (let ((cl (if thes thes (remove-duplicates data :test test)))
	(histo (list)))
    (dolist (c cl)
      (push (list c (length (remove-if-not #'(lambda (x) (funcall test c x)) data)))
	    histo))
    (if (not (member nil (mapcar #'numberp cl)))
        (sort histo '< :key 'car)
      (sort histo '> :key 'cadr))))

;;;
;; info theory

(defparameter *e* 2.7182818284590452353602874) ; (/ 848456353 312129649) = (exp 1)

#-Lispworks
(defun e-shannon (data &optional (base nil) (test #'equalp))
  "Returns the shannon entropy value of data.
Shannon entropy value may be between 0 and (log n base)."
  (let* ((cl (remove-duplicates data :test test))
	 (n (length data))
	 (p nil)
	 (b (if (numberp base)
		base (if (eql base 'e)
			 (exp 1)
			 (max 2 (length cl))))))
    (dolist (ci cl (- 0 (apply #'+ (mapcar #'(lambda (x) (* x (log! x b))) p))))
      (push (/ (length (remove-if-not #'(lambda (x) (funcall test x ci)) data)) n)
            p))))

#+Lispworks
(defun e-shannon (data &optional (base nil) (test #'equalp))
  "Returns the shannon entropy value of data.
Shannon entropy value may be between 0 and (log n base)."
  (let* ((cl (remove-duplicates data :test test))
	 (n (length data))
	 (p nil)
	 (b (if (numberp base)
		base (if (eql base 'e)
			 (exp 1)
			 (max 2 (length cl))))))
    (dolist (ci cl (- 0 (sum-loop(mapcar #'(lambda (x) (* x (log! x b))) p))))
      (push (/ (length (remove-if-not #'(lambda (x) (funcall test x ci)) data)) n)
            p))))

(defun windowed-e-shannon (data n &optional (base 2) (test #'equalp))
  "Applique e-shannon sur fenetres de taille n echantillons"
  (let* ((L (length data))
	 (w (/ L n)) )
    (loop for i from 0 to (1- n) ;(floor (/ L step)))
       collect (let ((k (floor (* (1+ i) w))))
		 (e-shannon (subseq data (floor (* i w)) (if (< k L) k L))
			    base test)))))

(defun shannon-entropy (data &optional (base nil) (samples nil) (test #'equalp))
  "Returns the entropy value of the data according to Shannon definition."
  (assert (or (integerp samples) (null samples)))
  (if (not samples)
      (e-shannon data base test)
      (windowed-e-shannon data samples base test)))

#-Lispworks
(defun kind-of-entropie-conditionnelle (data &optional (base 2) (test #'equalp))
  "Kind of conditionnal entropy based on a hack of shannon entropy by me. Experimental."
  (let* ((p nil) (len (length data))
	 (s (remove-duplicates data :test #'equalp)) ;test))
	 (n (length s))
	 (b (if (numberp base)
		base (if (eql base 'e) (exp 1) (max 2 n))))
	 (r (mapcar (lambda (x) (cons x (mapcar (lambda (y) (list y 0)) s))) s)))
    (dotimes (d (1- (length data))) ;build table of P(a if b)
      (let ((k (assoc (nth d data) r :test test))
	    (l (nth (1+ d) data)))
	(incf (cadr (assoc l (cdr k) :test test)) )))
    (dolist (i r (- 0 (apply #'+ (mapcar #'(lambda (x) (* x (log! x (* b b)))) p)))) ; sqr log to scale
      (dolist (j (cdr i))
	(push (/ (cadr j) len) p))))) ; P(i)

#+Lispworks
(defun kind-of-entropie-conditionnelle (data &optional (base 2) (test #'equalp))
  "Kind of conditionnal entropy based on a hack of shannon entropy by me. Experimental."
  (let* ((p nil) (len (length data))
	 (s (remove-duplicates data :test #'equalp)) ;test))
	 (n (length s))
	 (b (if (numberp base)
		base (if (eql base 'e) (exp 1) (max 2 n))))
	 (r (mapcar (lambda (x) (cons x (mapcar (lambda (y) (list y 0)) s))) s)))
    (dotimes (d (1- (length data))) ;build table of P(a if b)
      (let ((k (assoc (nth d data) r :test test))
	    (l (nth (1+ d) data)))
	(incf (cadr (assoc l (cdr k) :test test)) )))
    (dolist (i r (- 0 (sum-loop (mapcar #'(lambda (x) (* x (log! x (* b b)))) p)))) ; sqr log to scale
      (dolist (j (cdr i))
	(push (/ (cadr j) len) p))))) ; P(i)

;(kind-of-entropie-conditionnelle (loop for i from  0 to 1000 collect (random 2)) nil)

(defun windowed-entropie-cond (data n &optional (base nil) (test #'equalp))
  "Applique e-shannon sur fenetres de taille n echantillons."
  (let* ((L (length data))
	 (w (/ L n)) )
    (loop for i from 0 to (1- n) ;(floor (/ L step)))
       collect (let ((k (floor (* (1+ i) w))))
		 (kind-of-entropie-conditionnelle (subseq data (floor (* i w)) (if (< k L) k L))
						  base test)))))

(defgeneric entropie-conditionnelle (data &optional base samples test)
  (:documentation "Kinf of conditionnal entropy based on Shannon entropy (hack)."))

(defmethod entropie-conditionnelle ((data list) &optional (base nil) (samples nil) (test #'equalp))
  (if samples
      (windowed-entropie-cond data (floor (/ (length data) samples)) base test)
      (kind-of-entropie-conditionnelle data base test)))


(defgeneric self-info (data elt &optional test)
  (:documentation "Amount of information of each element <elt> in data.
Element <elt> is a symbol or a numeric value in a discrete space (for instance: binary).
If <elt> is null, self-info returns the amount of information for each element in data,
sorted in decreasing order of amount of information in data."))

(defmethod self-info ((data list) (elt symbol) &optional (test #'equalp))
  (when (not test) (setf test #'equalp))
  (- 0 (log (/ (length (remove-if-not #'(lambda (x) (funcall test x elt)) data)) (length data)) 2)))

(defmethod self-info ((data list) (elt integer) &optional (test #'equalp))
  (when (not test) (setf test #'equalp))
  (- 0 (log (/ (length (remove-if-not #'(lambda (x) (funcall test x elt)) data)) (length data)) 2)))

(defmethod self-info ((data list) (elt null) &optional (test #'equalp))
  (let ((elts (remove-duplicates data :test #'equalp)))
    (sort (mapcar #'(lambda (x) (list x (self-info data x test))) elts) '> :key #'second)))

;;; series
;; some tools for series

(defun xt2dt (xt)
  (let ((d (list (pop xt))))
    (dolist (x xt (reverse d))
      (when (not (= (cadr x) (cadar d)))
        (push x d)))))

(defun nocons= (seq &optional (test #'=))
  "Reechantillonne seq en y interpolant lineairement les valeurs consecutives egales :
each run of values equal (by test) to its first value x is replaced by a ramp from x to the
next different value; the last run is kept. seq is not modified."
  (let* ((v (coerce seq 'vector))
         (l (length v))
         (r (copy-seq v))
         (k 0))
    (loop while (< k l)
          do (let ((z (loop for n from 1
                            while (and (< (+ k n) l) (funcall test (aref v (+ k n)) (aref v k)))
                            finally (return n))))
               (when (and (> z 1) (< (+ k z) l))
                 (let ((d (float (/ (- (aref v (+ k z)) (aref v k)) z))))
                   (dotimes (n z)
                     (setf (aref r (+ k n)) (+ (aref v k) (* n d))))))
               (incf k z)))
    (coerce r 'list)))

;;; shell utils

#-Lispworks
(defun shell-command (command &rest args)
  "Runs COMMAND (a string or a list of program and arguments), see uiop:run-program for ARGS."
  (apply #'uiop:run-program command args))

#+Lispworks
(defun shell-command (string &rest args)
  (eval `(system:run-shell-command ,string .,args)))

;(shell-command "ls")


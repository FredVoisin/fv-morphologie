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

;;;; marks.lisp
;;;;
;;;; Contrast analysis (marks), mark structures and inner dynamic.

(in-package :fv-morphologie)

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
  ;; the bounds are fresh uninterned symbols: '*xstart* and '*xstop* were mistaken for
  ;; the same symbols in seq
  (let* ((seq+ (cons (make-symbol "START") (append seq (list (make-symbol "STOP")))))
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

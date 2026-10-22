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

;; fv-morphologie
;; to import data from files or export to files

(in-package :fv-morphologie)

;;; Date & Time

(defparameter *short-months*
  #("Jan" "Feb" "Mar" "Apr" "May" "June" "Jul" "Aug" "Sept" "Oct" "Nov" "Dec"))

(defun time-string ()
  (multiple-value-bind (s m h) (get-decoded-time)
    (format nil "~A:~2,,,'0@A:~2,,,'0@A" h m s)))

(defun date-string ()
  "Today's date, as a string such as \"22 Oct 2026\"."
  (multiple-value-bind (ig no re d mo y) (get-decoded-time)
    (declare (ignore ig no re))
    (format nil "~A ~A ~A" d (svref *short-months* (1- mo)) y)))

(defun date+time-string (&optional (u (get-universal-time)))
  "The date and time of the universal time u (now by default), as a string such as \"22 Oct 2026 14:05:09\"."
  (multiple-value-bind (s m h d mo y) (decode-universal-time u)
    (format nil "~A ~A ~A ~A:~2,,,'0@A:~2,,,'0@A"
                d (svref *short-months* (1- mo)) y h m s)))

;;; FILE SYSTEM

;(defconstant whitechars '#(#\Space #\Tab #\Return #\Newline))

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

(defun read-text-lines (file)
  "The lines of the text file, as strings."
  ;; read-line until the end of the file: file-position counts bytes, not characters
  (with-open-file (in-stream file
			     :direction :input
                             :element-type 'character)
    (loop for line = (read-line in-stream nil nil)
          while line
          collect line)))

(defun slurp-stream (stream)
  "The whole contents of the character stream, as a string."
;src: www.ymeme.com/slurping-a-file-common-lisp-83.html
;www.emmett.ca/~sabetts/slurp.html
  ;; file-length counts bytes: with multi-byte characters (UTF-8), fewer characters are
  ;; read, and only these are kept
  (let* ((l (file-length stream))
         (seq (make-string l)))
    (subseq seq 0 (read-sequence seq stream :end l))))

(defun slurp-file (file)
  (with-open-file (in file :direction :input)
    (slurp-stream in)))
 
(defun read-txt-file (file &key (mode t) (marks nil) (rem-test nil))
  "Reads the text file: mode t, each line as a list; nil, one flat list of strings; :charcode, the
segments of the file as lists of character codes. marks are the separators, besides blanks (spaces,
tabulations, newlines and other characters of code up to 32): a string, a list of characters or codes,
t for the usual punctuation marks, or, with :charcode, (:ignore c ...) for all the characters which are
not letters or digits, but c ..."
  (if (not file) nil
    (progn
      (when (not rem-test) (setf rem-test #'(lambda (x) (declare (ignore x)) nil)))
      (when (eq 'T marks) (setf marks " ,;:?./@&\"'(!)-_#*+=<>{}[]$`%^"))
      (unless (member mode '(t nil :charcode))
        (error "read-text: unknown mode ~S, use t (lines), nil (flat) or :charcode." mode))
      (if mode
          (case mode
            (:charcode
             (flet ((blankp (c) (<= c 32))
                    (letterp (c) (alphanumericp (code-char c))))
               (remove-if rem-test
                          (split-list-using-marks
                           (string2code (slurp-file file))
                           (cond ((stringp marks)
                                  (let ((codes (string2code marks)))
                                    #'(lambda (c) (or (blankp c) (member c codes)))))
                                 ((and (listp marks) (eql :ignore (car marks)))
                                  (let ((kept (all2char-code (cdr marks))))
                                    #'(lambda (c) (not (or (letterp c) (member c kept))))))
                                 ((listp marks)
                                  (let ((codes (all2char-code marks)))
                                    #'(lambda (c) (or (blankp c) (member c codes)))))
                                 (t (error "read-text: :sep must be a string or a list, not ~S." marks)))))))
	    ;; rem-test on the words of each line too (it was ignored in this mode)
	    (t (mapcar #'(lambda (x) (remove-if rem-test (string2sym (split-string x marks))))
		       (remove-if #'(lambda (x) (zerop (length x)))
				  (read-text-lines file)))))
	  (let ((st (slurp-file file)))
       	;(split-string st marks)
        ;(mapcar #'read-from-string
	    (remove-if rem-test (split-string st marks)) )))))

#|
(read-from-string (code2string (string2code "aiuo")))
(mapcar #'code2string
        (split-list-using-marks
         (subseq
         (string2code
          (slurp-file "~/txt/Deleuze/DR_314-315.txt")
          )
         3680 3706)
         (string2code "\\ ,?!.;:+=\"'(@)\^\&")
         )
        )

(char-code #\) ;195
(code-char 233)
|#

;;----------------
;; OUTPUT

(defun 2flatstring (liste s)
  (if (not liste) s
   (let ((k (pop liste)))
      (if (atom k)
          (2flatstring liste (if (zerop (length s))
                                (if (or (pathnamep k) (stringp k))
                                    (format nil "~G" k)
                                  (format nil "~S" k))
                                (funcall #'concat (list s " " (if (or (pathnamep k) (stringp k))
                                                                  (format nil "~G" k)
                                                                (format nil "~S" k))))))
        (2flatstring (append k liste) s)))))

(defun list-write (liste file mode)
  "Writes a list to a file."
  (with-open-file (out-stream (if file file "liste")
                              :direction :output
                              :element-type 'character
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (case mode
      (:coll (coll-entries liste out-stream))
      (:lisp
       (format out-stream "(~&")
       (dolist (l liste (format out-stream "~&)"))
         (format out-stream "~&~S" l)))
      (t (dolist (l liste (format out-stream "~&"))
           (if (atom l)
               (if (or (pathnamep l) (stringp l))
                   (format out-stream "~&~G" l)
                 (format out-stream "~&~S" l))
             (format out-stream "~&~G" (2flatstring l "")))))))
  (format nil "~&~G" file))

(defun coll-atom (x)
  "x as a Max coll item: integers as they are, other numbers as decimals, symbols by name in lower
case (Max symbols are case-sensitive), unless the name has lower-case letters (|Ab| stays Ab), and
quoted if they contain spaces, commas or semicolons; strings quoted, as they are."
  (flet ((quoted (str) (with-output-to-string (o)
                         (write-char #\" o)
                         (loop for c across str
                               do (when (member c '(#\" #\\)) (write-char #\\ o))
                                  (write-char c o))
                         (write-char #\" o))))
    (cond ((integerp x) (format nil "~D" x))
          ((realp x) (let ((*read-default-float-format* 'double-float))
                       (format nil "~F" (if (floatp x) x (float x)))))
          ((stringp x) (quoted x))
          ((symbolp x) (let ((name (let ((n (symbol-name x)))
                                     (if (string= n (string-upcase n)) (string-downcase n) n))))
                         (if (find-if #'(lambda (c) (member c '(#\Space #\Tab #\, #\; #\"))) name)
                             (quoted name)
                             name)))
          (t (quoted (princ-to-string x))))))

(defun coll-entries (liste stream)
  "Writes liste to stream as the entries of a Max coll: one line index, data; for each element,
from index 0; an element which is a list is flattened."
  (loop for e in liste
        for i from 0
        do (format stream "~D,~{ ~A~};~%" i
                   (mapcar #'coll-atom (if (listp e) (flat e) (list e))))))

(defun coll-write (liste file)
  "Writes a list to a Max coll file."
  (with-open-file (out-stream (if file file "coll")
                              :direction :output
                              :element-type 'character
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (coll-entries liste out-stream))
  (format nil "~&~G" file))

;;; eof

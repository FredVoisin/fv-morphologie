;; fv-morphologie
;; to import data from files or export to files

(in-package :fv-morphologie)

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
  (with-open-file (in-stream file
			     :direction :input
                             :element-type 'character)
    (loop with length = (file-length in-stream)
          while (< (file-position in-stream) length)
          collect (read-line in-stream))))

(defun slurp-stream (stream)
;src: www.ymeme.com/slurping-a-file-common-lisp-83.html
;www.emmett.ca/~sabetts/slurp.html
  (let* ((l (file-length stream))
         (seq (make-string l)))
    (read-sequence seq stream :end l)
    seq))

(defun slurp-file (file)
  (with-open-file (in file :direction :input)
    (slurp-stream in)))

(defun read-txt-file (file &key (mode t) (marks nil) (rem-test nil) (8bits nil))
  (if (not file) nil
    (progn
      (when (not rem-test) (setf rem-test #'(lambda (x) (declare (ignore x)) nil)))
      (when (not marks) (setf marks " "))
      (when (eq 'T marks) (setf marks " ,;:?./@&\"'(!)-_#*+=<>{}[]$`%^"))
      (if mode
          (case mode
            (:charcode
	     (cond ((eq T marks)
		    (remove-if rem-test
			       (split-list-using-marks
				(string2code (slurp-file file))
				#'(lambda (x) (not (alphanump x nil 8bits))))))
		   ((null marks)
		    (remove-if rem-test
			       (remove-if-not #'(lambda (x) (alphanump x '(32) 8bits))
					      (string2code (slurp-file file)))))
		   ((stringp marks)
		    (remove-if rem-test
			       (split-list-using-marks
				(string2code (slurp-file file))
				(string2code marks))))
		   ((listp marks)
		    (if (eql :ignore (car marks))
			(remove-if rem-test
			       (split-list-using-marks
				(string2code (slurp-file file))
				#'(lambda (x) (not (alphanump x
							      (all2char-code (cdr marks))
							      8bits)))))
			(remove-if rem-test
			       (split-list-using-marks
				(string2code (slurp-file file))
				(all2char-code marks)))))
		   (t (message "read-txt-file: marks must be a string or a list.")
		      (abort))))
	    (t (mapcar #'(lambda (x) (string2sym (split-string x marks)))
                                    (remove-if #'(lambda (x) (zerop (length x)))
                                               (read-text-lines file)))))
        (let ((st (slurp-file file)))
          ;(split-string st marks)
          ;(mapcar #'read-from-string
          (remove-if rem-test (split-string st marks)) ))))

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

(defun coll-write (liste file)
  "Writes a list to a Max coll file."
  (with-open-file (out-stream (if file file "coll")
                              :direction :output
                              :element-type 'character
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (dotimes (i (length liste))
      (format out-stream "~&~S," i)
      (dolist (e (nth i liste))
        (format out-stream " ~S" e))
      (format out-stream ";")))
  (format nil "~&~G" file))

;;; eof

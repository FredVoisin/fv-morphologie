;; to convert ShematicMind 'xml' to Freemind .mm
;; Fred Voisin, Dijon 2013
;; to be used with sm2mm.sh

(load "/usr/local/lisp/ftools.lisp")

(defun glue (tr br)
  (if (atom tr)
      tr
      (if (eq (car tr) (car br))
	  (append tr (list (list (cadr br) (cddr br))))
	  (cons (glue (car tr) br)
		(glue (cdr tr) br)))))

;(print (glue '((1 (a) (2 (b)) (3 (c) (4 (d))))) '(1 (5 (e)))))
;(print (glue '((1 (a) (2 (b)) (3 (c) (4 (d))))) '(2 (5 (e)))))
;(setf txtfile "/Users/fred/fred/admin/pro13-14/eme/sm2mm/emc12122013.tmp")
;(setf data (read-file-lines txtfile))

(defun links2tree (links)
  "Converts a list of links (X Y item) to a tree (X (Y item)).
See sm2mm.sh for preprocessing xml from schematicmind."
  (let ((r))
    (setf links (remove 'nil (mapcar #'(lambda (x) (mapcar #'read-from-string (segment-words x " "))) links))
	  r (list (list (caar links) (cdr (pop links)))))
    (loop while links do
	 (let ((y (pop links)))
	   (setf r (glue r y)) ))
    (values r)))

(defun traverse (list)
  (when list
    (let ((first (first list))
          (rest (rest list)))
      (if (atom first)
          (format t "~a " first)
        (traverse first))
      (traverse rest))))

;(traverse (links2tree data))

(defun traverse2 (list stream)
  (when list
    (let ((first (first list))
          (rest (rest list)))
      (if (atom first)
          (when (not (integerp first))
	    (format stream "\"~(~a~)\">" first))
	  (progn
	    (when (not (numberp (car first)))
	      (format stream "~&<node TEXT="))
	    (traverse2 first stream)))
      (progn
	(when (not (atom first))
	  (when (numberp (car first))
	    (when (plusp (car first))
	      (format stream "</node>"))))
	(traverse2 rest stream)))))

;(traverse2 (links2tree data) t)

(defun links2mm (infile outfile)
  (let ((tree (links2tree (read-file-lines infile))))
    (with-open-file (out outfile
			 :direction :output
			 :element-type 'base-char
			 :if-exists :supersede)
      (format out "<!-- generated with sm2mm.lisp -->~&")
      (format out "<map>~&<node TEXT=")
      (traverse2 tree out)
      (format out "~&</map>~&")))
  (values t))

;(links2tree (read-file-lines "/Users/fred/fred/admin/pro13-14/eme/sm2mm/emc12122013.tmp"))
;(links2mm "/Users/fred/fred/admin/pro13-14/eme/sm2mm/emc12122013.tmp" "/Users/fred/fred/admin/pro13-14/eme/sm2mm/reretest.mm")
;(progn (print (links2mm (links2mm \"/Users/fred/fred/admin/pro13-14/eme/sm2mm/emc12122013.tmp\" \"/Users/fred/fred/admin/pro13-14/eme/sm2mm/reretest.mm\"))) (quit))




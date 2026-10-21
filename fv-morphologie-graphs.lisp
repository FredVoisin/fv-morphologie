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

;; graphs to dot format for drawing (with neato)

;;; at first, to update morphologie 'draw-tree', which was representing undirected graphs from
;;; Thanks to neato program, the job is to convert list of (node-A node-B dist) into dot format

(in-package :fv-morphologie)

(defun make-name-from-path (path)
  "The name of the file path (without directory and type), for naming the graph."
  (or (and (or (pathnamep path) (stringp path))
           (pathname-name (pathname path)))
      "span-graph"))

(defun dot-id (x)
  "x as a quoted DOT identifier: strings as they are, other objects printed without package prefix."
  (let ((s (if (stringp x) x (let ((*package* (if (symbolp x) (or (symbol-package x) *package*) *package*)))
                                (princ-to-string x)))))
    (with-output-to-string (o)
      (write-char #\" o)
      (loop for c across s
            do (when (member c '(#\" #\\)) (write-char #\\ o))
               (write-char c o))
      (write-char #\" o))))

(defun dot-shape (shape)
  "A Graphviz shape name, case kept (Mrecord, box3d...)."
  (if (stringp shape) shape (string-downcase (string shape))))

(defun 2dot (graph min dis shape1 shape2 out legend &key (name "span-graph"))
  (let ((date (date))
        (nid (remove-if #'(lambda (x) (zerop (third x))) graph))
        (ide (remove-if-not #'(lambda (x) (zerop (third x))) graph)))
    (format out "graph ~A {~&" (dot-id (make-name-from-path name)))
    (format out "/* generated with fv-morphologie:graph>dot ~S ~D ~:R at ~Sh~S */~&"
            (first date) (nth (1- (cadr date)) *months*) (third date) (fourth date) (fifth date))
    (format out " node [shape=~A, font=Helvetica, fontsize=12, color=black, fontcolor=black];~&"
	    (dot-id (dot-shape shape1)))
    (format out " edge [font=Helvetica,fontsize=10, color=black, labelfloat=true];~&")
    (dolist (n nid)
      ;; len: the length itself (it was formatted on 3 characters first: 12.5 gave 13.)
      (format out " ~A -- ~A [w=~A, len=~,6F, label=~A];~&"
              (dot-id (car n)) (dot-id (cadr n)) dis (float (third n))
              (dot-id (if legend (third n) " "))))
    (when ide
      (format out " node [shape=~A, font=Helvetica, fontsize=12, color=blue, fontcolor=blue];~&"
	      (dot-id (dot-shape shape2)))
      (format out " edge [font=Helvetica,fontsize=10, color=blue];~&")
      (dolist (n ide)
        (format out " ~A -- ~A [len=~,6F];~&" (dot-id (car n)) (dot-id (cadr n)) (float min))))
    (format out "}~&")))

(defun graph-min-length (graph k)
  "The length drawn for edges of length 0: the shortest other length times k (k if there is none)."
  (let ((lengths (remove-if #'zerop (mapcar #'third graph))))
    (* (if lengths (reduce #'min lengths) 1) k)))

#-Lispworks
(defun graph2dot (graph dis mode shape out &optional (legend t))
  (let* ((k (if (numberp mode) mode (if mode 1 0.2)))
         (min (graph-min-length graph k))
         shape1 shape2)
    (when (not out) (setf out t))
    (if (not shape) (setf shape1 "ellipse" shape2 "ellipse")
      (if (stringp shape) (setf shape1 shape shape2 shape)
        (setf shape1 (car shape) shape2 (cadr shape))))
    (cond ((or (pathnamep out) (stringp out))
           (let ((file (pathname-name out))
                 (dir (pathname-directory out))
                 (type (pathname-type out)))
             (with-open-file (stream (make-pathname :directory dir
                                                    :name file
                                                    :type "dot")
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
               (2dot graph min dis shape1 shape2 stream legend :name out))
             (when (not (equalp type "dot"))
               ;; the type of the file is the output format of neato (svg, pdf, png, gif...)
               (let ((fmt (string-downcase (or type "png")))
                     (dot (namestring (make-pathname :directory dir :name file :type "dot"))))
                 (handler-case
                     (shell-command (list "neato"
                                          (format nil "-T~A" fmt)
                                          "-o" (namestring (make-pathname :directory dir
                                                                          :name file
                                                                          :type fmt))
                                          dot)
                                    ;; neato's messages to the terminal of the Lisp
                                    ;; (:error-output t is not supported by ECL)
                                    :error-output :interactive)
                   (error (c)
                     (error "graph>dot: the dot file ~A is written, but neato (Graphviz) could not make the ~A file: ~A"
                            dot fmt c)))))))
          (t (2dot graph min dis shape1 shape2 out legend :name out))))
  (format nil "~G" out))

#+Lispworks
(defun graph2dot (graph dis mode shape out &optional (legend t))
  (let* ((k (if (numberp mode) mode (if mode 1 0.2)))
         (min (graph-min-length graph k))
         shape1 shape2
         (neato "/usr/local/bin/neato"))
    (when (not out) (setf out t))
    (if (not shape) (setf shape1 "ellipse" shape2 "ellipse")
      (if (stringp shape) (setf shape1 shape shape2 shape)
        (setf shape1 (car shape) shape2 (cadr shape))))
    (cond ((or (pathnamep out) (stringp out))
           (let* ((file (pathname-name out))
                  (dir (pathname-directory out))
                  (type (pathname-type out))
                  (dot (make-pathname :directory dir
                                      :name file
                                      :type "dot")))
             (with-open-file (stream dot
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
               (2dot graph min dis shape1 shape2 stream legend :name out))
             (when (not (equalp type "dot"))
               (system::run-shell-command
                (list neato (format nil "-T~(~S~)" (if type (read-from-string type) (read-from-string "gif"))))
                :input dot
                :output (namestring (make-pathname :directory dir
                                           :name file
                                           :type (if type type "gif")))
                :wait t 
                :if-input-does-not-exist :error
                :if-output-exists :supersede
                ))))
          (t (2dot graph min dis shape1 shape2 out legend :name out)))
    (format nil "~G" out)))

;(graph2dot '((a b 1) (a c 2) (a d 3) (b k 8) (b j 0)) 0 .1 nil t)
;(graph2dot '((8 10 0) (1 8 1.5) (7 9 0) (7 11 1) (1 4 0) (1 11 1) (2 11 2) (2 6 0) (2 5 0) (2 3 0) (0 2 1)) 0 1 nil t)
;(graph2dot '((8 10 0) (1 8 1.5) (7 9 0) (7 11 1) (1 4 0) (1 11 1) (2 11 2) (2 6 0) (2 5 0) (2 3 0) (0 2 1)) 0 nil nil t)
;(graph2dot '((8 10 0) (1 8 1.5) (7 9 0) (7 11 1) (1 4 0) (1 11 1) (2 11 2) (2 6 0) (2 5 0) (2 3 0) (0 2 1)) 0 nil "ellipse" t)
;(graph2dot '((8 10 0) (1 8 1.5) (7 9 0) (7 11 1) (1 4 0) (1 11 1) (2 11 2) (2 6 0) (2 5 0) (2 3 0) (0 2 1)) 0 .25 "plaintext" t)
;(graph2dot '((8 10 0) (1 8 1.5) (7 9 0) (7 11 1) (1 4 0) (1 11 1) (2 11 2) (2 6 0) (2 5 0) (2 3 0) (0 2 1)) 0 1 '("point" "plaintext") t)
;(graph2dot '((8 10 0) (1 8 1.5) (7 9 0) (7 11 1) (1 4 0) (1 11 1) (2 11 2) (2 6 0) (2 5 0) (2 3 0) (0 2 1)) 0 1 "plaintext" "/Users/fred/g.gif")

(defgeneric graphtodot (graph dis min out &key shape legend)
  (:documentation "Converts a graph (list of edges) into an undirected graph into dot langage to be processed using neato program.
Arguments are:
graph : the graph
dis : amount of distortion allowed for distances fir drawing the graph (from 0.0 for minimum distorsion to 1.0 maximum distorsion)
min : scaling factor for the mininum distance
shape : string or list of strings to define node's shape, using 'neato' definitions
out :
if NIL or T, print out in listener;
if a string, it defines the pathname of the output. Type of file define output.
"))

(defmethod graphtodot  ((graph list) (dis null) (min null) (out t) &key shape legend)
  (graph2dot graph 0 nil shape out legend))

(defmethod graphtodot  ((graph list) (dis number) (min symbol)  (out t) &key shape legend)
  (graph2dot graph dis T shape out legend))

(defmethod graphtodot  ((graph list) (dis number) (min number)  (out t) &key shape legend)
  (graph2dot graph dis min shape out legend))


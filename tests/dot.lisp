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

(in-package :fv-morphologie/tests)

(def-suite dot :in fv-morphologie
  :description "Graphs drawn in the DOT format (neato).")
(in-suite dot)

(test graph>dot-stream
  (let ((dot (with-output-to-string (s) (graph>dot '((a b 1) (b c 0)) s))))
    (is (search "graph \"span-graph\" {" dot))
    (is (search "\"A\" -- \"B\"" dot))
    (is (search "\"B\" -- \"C\"" dot))))

(test graph>dot-dis-argument
  ;; used to refer to an undefined variable DISTORSION
  (finishes (with-output-to-string (s) (graph>dot '((a b 1)) s :dis 0.5))))

(test graph-name-from-path
  ;; a path without "/" crashed, and blanks cut the name
  (is (equal "out" (fv-morphologie::make-name-from-path "out.dot")))
  (is (equal "out" (fv-morphologie::make-name-from-path "out")))
  (is (equal "my graph" (fv-morphologie::make-name-from-path "my graph.dot")))
  (is (equal "c" (fv-morphologie::make-name-from-path "a.b/c.dot")))
  (is (equal "y" (fv-morphologie::make-name-from-path #p"/x/y.png")))
  (is (equal "span-graph" (fv-morphologie::make-name-from-path t))))

(test graph>dot-output
  (flet ((dot (graph &rest keys)
           (with-output-to-string (s) (apply #'graph>dot graph s keys))))
    ;; len was formatted on 3 characters first: 12.5 gave 13., 0.123456 gave .12
    (let ((d (dot '((a b 12.5) (b c 0.123456) (c d 1/3)))))
      (is (search "len=12.500000" d))
      (is (search "len=0.123456" d))
      (is (search "len=0.333333" d))
      ;; labels are quoted (1/3 unquoted is not valid DOT)
      (is (search "label=\"1/3\"" d)))
    ;; the case of the shape is kept (Mrecord gave mrecord)
    (is (search "shape=\"Mrecord\"" (dot '((a b 1)) :shape "Mrecord")))
    ;; no package prefix for the names of the nodes
    (let ((*package* (find-package :cl-user)))
      (is (search "\"A\" -- \"B\"" (dot (list (list 'fv-morphologie/tests::a 'fv-morphologie/tests::b 1))))))
    ;; quotes in names are escaped
    (is (search "\"q\\\"t\"" (dot '(("z" "q\"t" 2)))))
    ;; only edges of length 0, or no edge, used to fail
    (is (search "\"A\" -- \"B\" [len=1.000000]" (dot '((a b 0) (b c 0)))))
    (is (search "graph \"span-graph\" {" (dot '())))))

(test graph>dot-neato
  ;; draws a graph with neato, if Graphviz is installed (else the test is skipped)
  (if (ignore-errors (uiop:run-program '("neato" "-V") :output nil :error-output nil) t)
      (uiop:with-temporary-file (:pathname tmp :type "svg" :keep nil)
        (let* ((svg (namestring tmp))
               (dot (namestring (make-pathname :type "dot" :defaults tmp))))
          ;; 1/3 as an unquoted label was a syntax error for Graphviz: no image was made
          (graph>dot '((a b 1) (a c 2.5) (b d 0) (c e 1/3)) svg :shape "Mrecord")
          (is (probe-file svg))
          (is (search "<svg" (uiop:read-file-string svg)))
          (ignore-errors (delete-file dot))))
      (skip "neato (Graphviz) is not installed")))

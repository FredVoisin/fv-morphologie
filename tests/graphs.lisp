(in-package :fv-morphologie/tests)

(def-suite graphs :in fv-morphologie
  :description "Graphs and dot output.")
(in-suite graphs)

(defparameter *chain* '((a b 1) (b c 2) (c d 1)))

(test graph-span
  ;; minimal spanning tree: edge (0 2 4) is dropped (edge order may vary)
  (let ((*standard-output* (make-broadcast-stream)))
    (is (null (set-exclusive-or '((0 1 1) (1 2 2) (2 3 1))
                                (graph-span (copy-tree '((0 1 1) (0 2 4) (1 2 2) (2 3 1))))
                                :test #'equal)))))

(test graph-path
  (is (equal '(a b c d) (graph-path 'a 'd *chain*))))

(test graph-length
  (is (= 4 (graph-length *chain*))))

(test graph-degree
  (is (= 2 (graph-degree 'b *chain*))))

(test graph-extrem
  (is (null (set-exclusive-or '(a d) (graph-extrem *chain*)))))

(test graph>dot-stream
  (let ((dot (with-output-to-string (s) (graph>dot '((a b 1) (b c 0)) s))))
    (is (search "graph \"span-graph\" {" dot))
    (is (search "\"A\" -- \"B\"" dot))
    (is (search "\"B\" -- \"C\"" dot))))

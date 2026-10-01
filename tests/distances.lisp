(in-package :fv-morphologie/tests)

(def-suite distances :in fv-morphologie
  :description "2. Evaluation: distances.")
(in-suite distances)

(test dist-euclidian
  (is (approx= 5.0 (dist-euclidian '(0 0) '(3 4))))
  (is (approx= 0.0 (dist-euclidian '(1 2) '(1 2)))))

(test dist-citybloc
  (is (= 7 (dist-citybloc '(0 0) '(3 4)))))

(test dist-hamming
  (is (approx= 0.5 (dist-hamming '(a b c d) '(a x c y))))
  (is (= 2 (dist-hamming '(a b c d) '(a x c y) nil))))

(test dist-edit
  (is (= 0 (dist-edit '(a b c) '(a b c))))
  (is (= 3 (dist-edit '(a b c) '(x y z))))
  (is (= 3 (dist-edit '(k i t t e n) '(s i t t i n g))))
  (is (= 3 (dist-edit "kitten" "sitting")))
  (is (approx= 0.5 (dist-edit '(a b c d) '(a b) :norm t))))

(test dist-edit-symmetry
  (is (= (dist-edit '(a b c d e) '(b c x))
         (dist-edit '(b c x) '(a b c d e)))))

(test dist-multi-edit
  (is (approx= 0.25 (dist-multi-edit '((a 1) (b 2)) '((a 1) (c 2)) 1))))

(test dist-structure
  (is (approx= 0.0 (dist-structure '(a b a b) '(a b a b)))))

(test dist-graph
  (is (= 4 (dist-graph 'a 'd '((a b 1) (b c 2) (c d 1))))))

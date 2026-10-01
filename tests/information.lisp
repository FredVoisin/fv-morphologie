(in-package :fv-morphologie/tests)

(def-suite information :in fv-morphologie
  :description "2. Evaluation: histogram and entropy.")
(in-suite information)

(test histogram
  (is (equal '((a 3) (b 2) (c 1)) (histogram '(a b a c a b)))))

(test entropy
  ;; entropy prints its mode to standard output
  (let ((*standard-output* (make-broadcast-stream)))
    (is (approx= 0.0 (entropy '(a a a a))))
    (is (approx= 1.0 (entropy '(a b a b))))
    (is (approx= 2.0 (entropy '(a b c d))))))

(test elt-info
  (is (approx= 1.0 (elt-info '(a b a c) 'a))))

(in-package :fv-morphologie/tests)

(def-suite classification :in fv-morphologie
  :description "3. Classification: marks, motifs and classes.")
(in-suite classification)

(test mark-position
  (is (equal '(0 3) (mark-position '(a b c a b) 'a))))

(test mark-list
  (is (equal '(((a b c) (0)) ((a b d) (3)))
             (mark-list '(a b c a b d) 'a))))

(test mark-structure
  (is (equal '((0 1 2) (0 1 2)) (mark-structure '(a b c a b d a b) nil))))

(test motif-find
  (is (equal '((0 1) (3 4) (6 7)) (motif-find '(a b) '(a b c a b d a b))))
  (is (equal '((0 1) (6 7)) (motif-find '(a b) '(a b c a c d a b))))
  (is (equal '((0 1) (3 4) (6 7)) (motif-find '(a b) '(a b c a c d a b) :diss 0.5))))

(test motif-list
  (is (equal '(((a b c) (0 2) (3 5) (7 9)))
             (motif-list '(a b c a b c d a b c) nil))))

(test motif-group
  (is (equal '((1 1) (2 2 2) 3) (motif-group '(1 1 2 2 2 3) #'=))))

(test class-num
  ;; two obvious clusters: the partition must separate them
  ;; (centroids start at random: use a fixed seed to keep the test deterministic)
  (let* ((*random-state* #+sbcl (sb-ext:seed-random-state 42) #-sbcl (make-random-state nil))
         (classes (first (class-num '((0 0) (0 1) (10 10) (10 11)) 2 :centroids))))
    (is (= (first classes) (second classes)))
    (is (= (third classes) (fourth classes)))
    (is (/= (first classes) (third classes)))))

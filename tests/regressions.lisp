(in-package :fv-morphologie/tests)

(def-suite regressions :in fv-morphologie
  :description "Bugs fixed while moving to ASDF: they must not come back.")
(in-suite regressions)

(test no-self-recursive-wrappers
  ;; used to loop forever
  (signals error (num>alpha 2.5))
  (signals error (alpha>num #\a)))

(test dist-euclid-number-list
  ;; used to map over the number instead of the list
  (is (equal '(1 4 2) (dist-euclid 1 '(2 5 -1))))
  (is (equal '(1 4 2) (dist-euclid '(2 5 -1) 1))))

(test split-list-uses-marks
  (is (equal '((a b) (c)) (split '(a b x c) '(x)))))

(test motif-find-uses-edit-costs
  ;; used to call find-pos with too many arguments
  (finishes (motif-find '(a b) '(a b c) :change 2 :ins 1 :del 1)))

(test motif-list-default-output
  ;; (set out :length) used to fail at compile time
  (finishes (motif-list '(a b a b) nil)))

(test dist-multi-edit-keywords
  ;; positional arguments used to be passed as keywords
  (finishes (dist-multi-edit '((a 1) (b 2)) '((a 1) (c 2)) 1 :sub 2)))

(test graph>dot-dis-argument
  ;; used to refer to an undefined variable DISTORSION
  (finishes (with-output-to-string (s) (graph>dot '((a b 1)) s :dis 0.5))))

(test date-string-months
  ;; MONTHS used to be undefined
  (is (search "Jan" (date+time-string (encode-universal-time 0 0 12 15 1 2000)))))

(test graph-span-max-distance-edges
  ;; used to loop forever when the tree needed an edge of maximum length,
  ;; and printed its input
  (let* ((segs '((a b c) (a b d) (x y z) (x y w) (a b c d)))
         (tree (graph-span (dist-edit segs nil :norm t))))
    (is (= 4 (length tree)))
    (is (equal "" (with-output-to-string (*standard-output*)
                    (graph-span (dist-edit segs nil :norm t)))))))

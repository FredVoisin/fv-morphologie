(in-package :fv-morphologie/tests)

(def-suite io :in fv-morphologie
  :description "4. Read/Write.")
(in-suite io)

(test write-list/read-text
  (uiop:with-temporary-file (:pathname file :type "txt")
    (write-list '((a b c) (1 2 3)) (namestring file))
    (is (equal '((a b c) (1 2 3)) (read-text (namestring file))))
    (is (equal '("A" "B" "C" "1" "2" "3") (read-text (namestring file) :mode nil)))))

(test dates
  (is (stringp (date-string)))
  (is (stringp (date+time-string))))

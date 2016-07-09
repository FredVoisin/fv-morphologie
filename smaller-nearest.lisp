(defun smaller-nearest (x liste)
  (let* ((dx (mapcar #'(lambda (e) (- e x)) liste))
         (min (apply #'min (mapcar #'abs dx)))
         (aliste (mapcar #'(lambda (x y) (list x y)) dx liste)))
    (cadar (sort (remove-if-not #'(lambda (a) (= min (abs a))) aliste :key #'car) '< :key #'car))))

(smaller-nearest 5 '(1 3 6 7 9)) ;=> 6
(smaller-nearest 5 '(6 4 7)) ;=> 4

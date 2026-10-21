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

(in-package :fv-morphologie)

(defvar diketo
  (read-text  "~/projets/fv-morphologie/exemples/data/diketo-db.txt"
	      :mode T :sep " " :rem-test nil))

; (car diketo) > TIME_S INTENSITY_DB

(setq diketo-t (mapcar #'car (cdr diketo)))
(setq diketo-a (mapcar #'cadr (cdr diketo)))

(length diketo-a) ;> 2467

(apply #'min diketo-a) ;> 18.88
(apply #'max diketo-a) ;> 46.95

(length (setq diketo-xs (exsample diketo-a nil nil 0))) ;> 587
(write-list diketo-xs "~/projets/fv-morphologie/exemples/data/diketo-xs")

(length (setq diketo-xs01 (exsample diketo-a :minmax :values 0.1))) ;> 515
(length (setq diketo-xs02 (exsample diketo-a :minmax :values 0.2))) ;> 481
(length (setq diketo-xs03 (exsample diketo-a :minmax :values 0.3))) ;> 453
(write-list diketo-xs03 "~/projets/fv-morphologie/exemples/data/diketo-xs03.dat")

(length (setq diketo-xs03t (exsample diketo-a :minmax :all 0.3))) ;> 453
(write-list diketo-xs03t "~/projets/fv-morphologie/exemples/data/diketo-xs03t.dat")

(length (setq diketo-xs8 (exsample diketo-a :minmax :all 8))) ;> 143
(write-list diketo-xs8 "~/projets/fv-morphologie/exemples/data/diketo-xsx.dat")
; taux de compression de l'échantillon :
(float (/ 143 2467)) ;= 0.24 ~ 0.18 ~ 0.05

(setq diketo-entropies-x
      (loop for s from 0 to 24 by 0.1
	 collect (let ((xs (exsample diketo-a :minmax :values s)))
		   (list s ;(length xs)
			 (e-shannon xs ;)
				    :test #'(lambda (x y)
					      (< (abs (- y x))
						 (/ (+ x y) 15)))
				    :base 2)))))


(write-list diketo-seuil "~/projets/fv-morphologie/exemples/data/diketo-seuil.dat")
(write-list diketo-entropies-x "~/projets/fv-morphologie/exemples/data/diketo-entropies-x.dat")

(histogram (mapcar #'cadr diketo-entropies-x)) ; :test #'(lambda (x y) (< (abs (- x y)) 0.1)))




;;;;;;;;

(defgeneric cut (seq &optional marks)
  (:documentation ""))

(defmethod cut ((seq string) &optional marks)
  (if marks
      (split-string seq marks)
      (split-string seq " ")))

(defmethod cut ((seq list) &optional marks)
  (split-list-using-marks seq marks))

(defgeneric data-import (data &rest options)
  (:documentation "Try to import data as structured lists or lists of lists, with some options to be developped."))

;(import file-or-strings #'(lambda (x) (represents x whatever)))



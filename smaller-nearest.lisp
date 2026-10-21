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

(defun smaller-nearest (x liste)
  (let* ((dx (mapcar #'(lambda (e) (- e x)) liste))
         (min (apply #'min (mapcar #'abs dx)))
         (aliste (mapcar #'(lambda (x y) (list x y)) dx liste)))
    (cadar (sort (remove-if-not #'(lambda (a) (= min (abs a))) aliste :key #'car) '< :key #'car))))

(smaller-nearest 5 '(1 3 6 7 9)) ;=> 6
(smaller-nearest 5 '(6 4 7)) ;=> 4

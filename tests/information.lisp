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

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

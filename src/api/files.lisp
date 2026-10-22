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

;;;; api/files.lisp
;;;;
;;;; Public functions: import and export (text files, lists, Max coll files).

(in-package :fv-morphologie)

(defgeneric read-text (file &key mode sep rem-test)
  (:documentation "READ-TEXT: reads text <file> using following <mode> formats (another mode is an error):
 T : each line of file as a list of symbols or numbers
 NIL : flat mode, reads file as a single list of strings
 :charcode : the segments of the file, as lists of character codes.
Additional keyword :sep is a string defining all characters defining a separator, besides blanks
(spaces, tabulations, newlines), or t for the usual punctuation marks; with :charcode, it may also
be a list of characters or codes, or (:ignore c ...) to separate at all the characters which are
not letters or digits (in any alphabet), but c ...
Additional Keyword :rem-test defines a function to remove choosen words."))

(defmethod read-text ((file t)  &key (mode t) (sep nil) (rem-test nil))
  (read-txt-file file :mode mode :marks sep :rem-test rem-test))

; (defmethod read-midi ((file))
; (defmethod read-sdif ((file))

(defgeneric display-list (list &optional recursive)
  (:documentation "DISPLAY-LIST: prints out to the Lisp listener each element of list within a newline with indentation relative to list level."))

(defmethod display-list (list &optional (recursive nil))
  (declare (ignore recursive))
  (list-display list))
  
(defgeneric write-list (list file &optional mode)
  (:documentation "WRITE-LIST: write list to a file using following optional argument <mode> format:
:flat (by default) : each element of list is written in a new line, sub-lists without parentheses (as read by read-text)
:lisp : list is written as a Lisp list
:coll : a Max coll file, one line index, data; for each element, from index 0."))

(defmethod write-list ((list list) (file t) &optional (mode :supersede))
  (list-write list file mode))

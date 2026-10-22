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
  :description "4. Read and write files.")
(in-suite io)

(test write-list/read-text
  (uiop:with-temporary-file (:pathname file :type "txt")
    (write-list '((a b c) (1 2 3)) (namestring file))
    (is (equal '((a b c) (1 2 3)) (read-text (namestring file))))
    (is (equal '("A" "B" "C" "1" "2" "3") (read-text (namestring file) :mode nil)))))

(test dates
  (is (stringp (date-string)))
  (is (stringp (date+time-string))))

(test date-string-months
  ;; MONTHS used to be undefined
  (is (search "Jan" (date+time-string (encode-universal-time 0 0 12 15 1 2000)))))

(test slurp-file-multibyte
  ;; file-length counts bytes: a UTF-8 file gave one NUL (SBCL) or space (ECL)
  ;; per extra byte at the end of the string
  (uiop:with-temporary-file (:pathname file :type "txt")
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (format out "~a~%~a" (coerce (list (code-char 233) #\t (code-char 233)) 'string) "abc"))
    (let ((s (fv-morphologie::slurp-file file))
          (lines (fv-morphologie::read-text-lines file)))
      (is (equal 2 (length lines)))
      (is (= (length s) (+ (reduce #'+ (mapcar #'length lines)) (1- (length lines)))))
      (is (notany #'(lambda (c) (member (char-code c) '(0))) s))
      (is (char= #\c (char s (1- (length s)))))))
  ;; an empty file
  (uiop:with-temporary-file (:pathname file :type "txt")
    (is (equal "" (fv-morphologie::slurp-file file)))
    (is (null (fv-morphologie::read-text-lines file)))))

(test read-text-charcode
  (flet ((write-file (file text)
           (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
             (write-string text out))))
    (uiop:with-temporary-file (:pathname file :type "txt")
      (write-file file (format nil "ab-cd~cef~%gh!ij~%" #\Tab))
      ;; newlines and tabulations separate the segments (they stayed inside them)
      (is (equal '((97 98 45 99 100) (101 102) (103 104 33 105 106))
                 (read-text file :mode :charcode)))
      (is (equal '((97 98) (99 100) (101 102) (103 104) (105 106))
                 (read-text file :mode :charcode :sep "-!")))
      (is (equal (read-text file :mode :charcode :sep "-!")
                 (read-text file :mode :charcode :sep (list #\- #\!))))
      (is (equal '((97 98 45 99 100) (101 102) (103 104) (105 106))
                 (read-text file :mode :charcode :sep (list :ignore #\-))))
      ;; the old PWGL modes, and any unknown mode, are errors (:ascii-7b gave the lines)
      (signals error (read-text file :mode :ascii-7b))
      (signals error (read-text file :mode :foo)))
    (uiop:with-temporary-file (:pathname file :type "txt")
      ;; accented letters are letters (they were separators without 8 bits)
      (write-file file (format nil "~a,~%~a~%"
                               (coerce (list (code-char 233) #\t (code-char 233)) 'string)
                               (coerce (list (code-char 231) #\a) 'string)))
      (is (equal '((233 116 233) (231 97)) (read-text file :mode :charcode :sep ",")))
      (is (equal '((233 116 233) (231 97)) (read-text file :mode :charcode :sep (list :ignore)))))))

(test write-list-coll
  ;; :coll wrote the default format, not a Max coll file (coll-write was never called)
  (uiop:with-temporary-file (:pathname file :type "txt")
    (write-list (list '(60 62 64) 72 '(a b c) '(1/3 2.5) '("hello world" x) '((1 2) (3 (4)))
                      '(|Ab| "Cd" |e f|))
                (namestring file) :coll)
    ;; symbols in lower case for Max, unless typed with lower-case letters; strings as they are
    (is (equal '("0, 60 62 64;" "1, 72;" "2, a b c;" "3, 0.33333334 2.5;"
                 "4, \"hello world\" x;" "5, 1 2 3 4;" "6, Ab \"Cd\" \"e f\";")
               (uiop:read-file-lines file))))
  ;; the default mode is unchanged
  (uiop:with-temporary-file (:pathname file :type "txt")
    (write-list '((a b c) (1 2 3)) (namestring file))
    (is (equal '("A B C" "1 2 3") (uiop:read-file-lines file)))))

(test read-text-rem-test-lines
  (uiop:with-temporary-file (:pathname file :type "txt")
    (with-open-file (out file :direction :output :if-exists :supersede)
      (format out "the cat sat~%on a mat~%it is~%"))
    ;; rem-test was ignored in lines mode (the default)
    (is (equal '((the cat sat) (mat) ())
               (read-text file :rem-test #'(lambda (w) (< (length (string w)) 3)))))
    (is (equal '("the" "cat" "sat" "mat")
               (read-text file :mode nil :rem-test #'(lambda (w) (< (length w) 3)))))
    ;; without rem-test, unchanged
    (is (equal '((the cat sat) (on a mat) (it is)) (read-text file)))))

(test read-without-eval
  ;; read-from-string evaluated #. : a word #.(...) of a text file ran its code (read-text, lines mode)
  (let ((ran nil))
    (declare (special ran))
    (setf (symbol-function 'read-without-eval-marker) #'(lambda () (setf ran t) 'injected))
    (uiop:with-temporary-file (:pathname file :type "txt")
      (with-open-file (out file :direction :output :if-exists :supersede)
        (format out "hello #.(fv-morphologie/tests::read-without-eval-marker) world~%a ) # (~%3 4.5 abc~%"))
      (let ((r (read-text file)))
        (is (not ran))
        ;; unreadable words are kept as strings
        (is (equal '(hello "#.(fv-morphologie/tests::read-without-eval-marker)" world) (first r)))
        (is (equal '(a ")" "#" "(") (second r)))
        (is (equal '(3 4.5 abc) (third r))))))
  (signals reader-error (fv-morphologie::safe-read-from-string "#.(+ 1 2)"))
  (is (= 3 (fv-morphologie::safe-read-from-string "3"))))

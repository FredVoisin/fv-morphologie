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

(def-suite help :in fv-morphologie
  :description "Documentation: menu, doc and help.")
(in-suite help)

(test help-and-doc
  (let ((*standard-output* (make-broadcast-stream)))
    ;; help printed every similarity score and returned nil
    (is (member 'graph-span (help 'graph)))
    (is (equal '(entropy) (help :entropy)))
    (is (null (help 'zzzzzz)))
    (is (every #'symbolp (help "distance")))
    ;; doc from another package: 'dist-edit was cl-user::dist-edit (nil)
    (let ((*package* (find-package :cl-user)))
      (is (stringp (doc (intern "DIST-EDIT" :cl-user))))
      (is (stringp (doc "histogram"))))
    (is (null (doc 'not-a-function))))
  ;; help prints the documentations of the functions found, not the scores
  (is (search "ENTROPY:" (with-output-to-string (*standard-output*) (help :entropy)))))

(test menu-lists-all-functions
  ;; 23 exported functions were missing from the menu: (doc) did not list them, help could not find them
  (let (missing)
    (do-external-symbols (s :fv-morphologie)
      (unless (member s fv-morphologie::*fv-morphologie-menu*) (push s missing)))
    (is (null missing)))
  (is (every #'fboundp fv-morphologie::*fv-morphologie-menu*))
  (is (every #'(lambda (f) (documentation f 'function)) fv-morphologie::*fv-morphologie-menu*))
  (let ((*standard-output* (make-broadcast-stream)))
    (is (member 'filt-median (help 'median)))))

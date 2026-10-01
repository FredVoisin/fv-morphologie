;;;; fv-morphologie.asd
;;;;
;;;; Common Lisp system definition for fv-morphologie.
;;;; (asdf:load-system :fv-morphologie)
;;;; (asdf:test-system :fv-morphologie)
;;;;
;;;; FiveAM is used only by the test system (fv-morphologie/tests),
;;;; to test the code: fv-morphologie itself does not depend on it.

(asdf:defsystem :fv-morphologie
  :description "Lisp tools to analyse musical and syntagmatic sequences as symbolic expressions."
  :author "Frederic Voisin"
  :version "0.1.0"
  :depends-on (:uiop)
  :serial t
  :components ((:file "package")
               (:file "fv-morphologie-encodage")
               (:file "fv-morphologie")
               (:file "fv-morphologie-graphs")
               (:file "fv-morphologie-io")
               (:file "fv-morphologie-cl"))
  :in-order-to ((test-op (test-op :fv-morphologie/tests))))

(asdf:defsystem :fv-morphologie/tests
  :description "Unit tests for fv-morphologie (FiveAM is used for testing only)."
  :depends-on (:fv-morphologie :fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "encodage")
               (:file "distances")
               (:file "information")
               (:file "classification")
               (:file "graphs")
               (:file "io")
               (:file "regressions"))
  :perform (test-op (op c)
             (unless (uiop:symbol-call :fiveam :run!
                                       (uiop:find-symbol* :fv-morphologie :fv-morphologie/tests))
               (error "fv-morphologie: some tests failed."))))

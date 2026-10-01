(in-package :fv-morphologie/tests)

(def-suite encodage :in fv-morphologie
  :description "1. Transcription: encoding and preprocessing.")
(in-suite encodage)

(test transcode
  (is (equal '(x b z x) (transcode '(a b c a) '((a x) (c z)))))
  (is (equal '() (transcode '() '((a x))))))

(test alpha>num
  (is (equal '(65 66 67) (alpha>num 'abc)))
  (is (equal '(97 98 99) (alpha>num "abc")))
  (is (= 72 (alpha>num "C" :midi)))
  (is (= 72 (alpha>num 'c :midi))))

(test num>alpha
  (is (equal '(a b ab) (num>alpha '(0 1 27)))))

(test num>base
  (is (= 1010 (num>base 10 2)))
  (is (eq 'ff (num>base 255 16))))

(test concaten
  (is (eq 'abc (concaten '(a b c))))
  (is (string= "abcd" (concaten '("ab" "cd"))))
  (is (eq 'x (concaten 'x)))
  (is (= 123 (list>sym '(1 2 3)))))

(test str->symb
  (is (equal '(a b c) (str->symb "a b c")))
  (is (equal '((a b) (c)) (str->symb '("a b" "c")))))

(test split
  (is (equal '("hello" "big" "world") (split "hello big world")))
  (is (equal '((a b) (c d) (e)) (split '(a b x c d x e) '(x)))))

(test filters
  (is (equal '(1 1 1 1 1 1 1) (filt-median '(1 9 1 1 1 9 1) 3)))
  (is (equal '(a b c a) (filt-local-rep '(a a b b b c a a)))))

# PWGL legacy

Material from the PWGL (Sibelius Academy) version of fv-morphologie, kept for
reference. It is not part of the Common Lisp system (`fv-morphologie.asd`).

- `fv-morphologie-pwgl.lisp`: the PWGL boxes (bindings to the library,
  in package `:dbl`, requires OMPW).
- `tutorial/`: PWGL patches (`.pwgl`, `.dbd`) presenting the library, in five
  sections: 1-Transcription, 2-Evaluation, 3-Classification, 4-ReadWrite and
  examples with data.

These patches need PWGL and the bindings in `fv-morphologie-pwgl.lisp`.

# PWGL legacy

Material from the PWGL (Sibelius Academy) version of fv-morphologie, kept for
reference. It is not part of the Common Lisp system (`fv-morphologie.asd`).

- `fv-morphologie-pwgl.lisp`: the PWGL boxes (bindings to the library,
  in package `:dbl`, requires OMPW).
- `tutorial/`: PWGL patches (`.pwgl`, `.dbd`) presenting the library, in five
  sections: 1-Transcription, 2-Evaluation, 3-Classification, 4-ReadWrite and
  examples with data.

These patches need PWGL and the bindings in `fv-morphologie-pwgl.lisp`.

## License

- `fv-morphologie-pwgl.lisp`: GPL-3.0-only, like the rest of the code (see
  [../LICENSE](../LICENSE)).
- `tutorial/`: Copyright (C) 2010-2011 Frederic Voisin, Creative Commons
  Attribution 4.0 International (CC BY 4.0), see
  [tutorial/LICENSE](tutorial/LICENSE).

**Exceptions:** the following example texts in `tutorial/5. FV-exemples/data/`
are by other authors. They are not covered by CC BY 4.0 and keep their own
status; they are included as short excerpts for analysis and teaching:

- `Alice.txt`: Lewis Carroll, *Alice's Adventures in Wonderland*, Project
  Gutenberg edition (public domain).
- `Difference&Repetition-ascii.txt`, `Difference&Repetition-utf8.txt`:
  excerpt from Gilles Deleuze, *Différence et répétition*, PUF, 1968.
- `Stalker.txt`: excerpt from Arkadi and Boris Strougatski, *Stalker
  (Pique-nique au bord du chemin)*, French translation.

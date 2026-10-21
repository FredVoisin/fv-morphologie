# fv-morphologie

Experimental tools to analyse musical and syntagmatic sequences as symbolic expressions, in Common Lisp

Analysis-oriented branch of the legacy library [Morphologie](https://github.com/openmusic-project/Morphologie) for OpenMusic (Ircam)
and PWGL (Mikael Laurson, Siblius Academy).


Documentation: [doc/fv-morphologie.md](doc/fv-morphologie.md).
The PWGL tutorial and bindings are kept in [pwgl-legacy/](pwgl-legacy/).

## Loading (Common Lisp)

fv-morphologie has no dependency besides ASDF/UIOP, which come with every
modern Common Lisp implementation.

### With Quicklisp

Clone the repository into Quicklisp's `local-projects` directory (or symlink
it there), then quickload it:

```sh
cd ~/quicklisp/local-projects/
git clone https://github.com/FredVoisin/fv-morphologie.git
```

```lisp
(ql:quickload :fv-morphologie)
(in-package :fv-morphologie)
(help)
```

If the system is not found after cloning, run `(ql:register-local-projects)`
once.

The public functions are exported, so they can also be used from another
package, e.g. `(fv-morphologie:dist-edit '(a b c) '(a c))`, or with
`(:use :cl :fv-morphologie)` in your own `defpackage`.

### With ASDF only

Put (or symlink) the project directory in a place ASDF looks into, such as
`~/common-lisp/`, then:

```lisp
(asdf:load-system :fv-morphologie)
```

### In your own system

```lisp
(asdf:defsystem :my-project
  :depends-on (:fv-morphologie)
  ...)
```

`graph>dot` needs Graphviz (`neato`) to produce `.png`/`.gif` output.

## Testing

The tests live in `tests/` and use [FiveAM](https://github.com/lispci/fiveam).
FiveAM is used only for testing the code: `fv-morphologie` itself does not
depend on it, only the `fv-morphologie/tests` system does.

```lisp
(ql:quickload :fv-morphologie/tests)  ; fetches FiveAM if needed
(asdf:test-system :fv-morphologie)
```

## License

Copyright (C) 1992-2026 Frederic Voisin.

fv-morphologie is derived from
[Morphologie](https://github.com/openmusic-project/Morphologie), released
under GPL-3.0.

- **Code** (`.lisp` and `.asd` files): GNU General Public License,
  version 3 only (GPL-3.0-only), see [LICENSE](LICENSE).
- **Documentation** (`doc/`): Creative Commons Attribution 4.0
  International (CC BY 4.0), see [doc/LICENSE](doc/LICENSE).

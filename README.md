# fv-morphologie

Experimental tools to analyse musical and syntagmatic sequences as symbolic expressions, in Common Lisp

Analysis-oriented branch of the legacy library [Morphologie](https://github.com/openmusic-project/Morphologie) for OpenMusic (Ircam)
and PWGL (Mikael Laurson, Siblius Academy).


Documentation: [doc/fv-morphologie.md](doc/fv-morphologie.md).
The PWGL tutorial and bindings are kept in [pwgl-legacy/](pwgl-legacy/).

## Loading (Common Lisp)

With ASDF, make the project directory visible (e.g. symlink it into
`~/quicklisp/local-projects/` or `~/common-lisp/`), then:

```lisp
(asdf:load-system :fv-morphologie)
(in-package :fv-morphologie)
(help)
```

`graph>dot` needs Graphviz (`neato`) to produce `.png`/`.gif` output.

## Testing

The tests live in `tests/` and use [FiveAM](https://github.com/lispci/fiveam).
FiveAM is used only for testing the code: `fv-morphologie` itself does not
depend on it, only the `fv-morphologie/tests` system does.

```lisp
(ql:quickload :fiveam)                ; once
(asdf:test-system :fv-morphologie)
```

## TODO

- `class-sym` does not always return the requested number of classes
  (e.g. 3 classes when 2 are asked for 7 segments, see
  `(class-sym '((a b c) (a b d) (x y z) (x y w) (a b c d) (k l m n o p) (k l m n o q)) 2 :edit-norm)`).
- `class-num` in `:1d-centroids` mode returns 2 classes only, whatever the
  number of classes requested (it uses a separate one-dimensional algorithm,
  `1d-class`).

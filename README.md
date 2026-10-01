# fv-morphologie

Lisp tools to analyse musical and syntagmatic sequences as symbolic expressions.
For Common Lisp, OpenMusic (Ircam) and PWGL (Sibelius Academy).



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

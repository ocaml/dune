Using the mdx stanza, you can set mdx prelude which are used to evaluate
code in the toplevel environments before checking a document. To do that
you can set the prelude field of the stanza: `(preludes <prelude_file> ...)`.

```ocaml
# x;;
- : int = 1
```

Different environment can have their own prelude. This is set using the
`(env <env_name> <prelude_file>)` syntax for the prelude field.
Here x and why are set in `alt.ml`.

```ocaml env=a
# x + y;;
- : int = 11
```

```ocaml env=b
# x + y;;
- : int = 21
```

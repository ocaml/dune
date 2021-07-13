Dependencies for file-include mdx blocks should be computed by dune, using
`ocaml-mdx deps`:

```ocaml file=example.ml
let x = 1
```

Same goes for mdx's `dir` labels:

```sh dir=stuff
$ ls
blah
bleh
blih
```

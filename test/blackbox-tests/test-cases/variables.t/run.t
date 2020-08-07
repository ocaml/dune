According to the doc: CC is the C compiler command line (list made of the
compiler name followed by its flags) that was used to compile OCaml in the
current build context.

In practice it consists in the concatenation of OCaml's `c_compiler` and
`ocamlc_cflags` config values.

  $ O_CC=$(ocamlc -config-var c_compiler)
  $ O_CCF=$(ocamlc -config-var ocamlc_cflags)

  $ dune build @cc | sed "s,${O_CC} ${O_CCF},OK,"
  OK

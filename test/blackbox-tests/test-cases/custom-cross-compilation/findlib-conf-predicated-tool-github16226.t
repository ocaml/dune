Reproduce #16226. A tool defined only for a Findlib toolchain causes Dune to
interpret its value as the empty string when initializing the default context.

  $ make_dune_project 2.7
  $ mkdir -p findlib.conf.d
  $ export OCAMLFIND_CONF=$PWD/findlib.conf
  $ cat >findlib.conf.d/solo5.conf <<EOF
  > ocamlmklib(solo5) = "/does/not/matter"
  > EOF

The base configuration file does not need to exist for Dune to load snippets
from the corresponding .d directory.

  $ dune build

An explicitly invalid tool name is still an error.

  $ cat >findlib.conf <<EOF
  > ocamlmklib = ""
  > EOF
  $ dune build
  Error: The effective Findlib configuration specifies an invalid program name
  "" for program "ocamlmklib".
  -> required by loading the OCaml compiler for context "default"
  [1]

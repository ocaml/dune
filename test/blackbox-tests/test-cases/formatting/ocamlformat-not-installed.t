Regression test that `dune build @fmt` exits cleanly if ocamlformat is not
installed and no `.ocamlformat` file is found.

See https://github.com/ocaml/dune/issues/10578 .

  $ cat >dune-project <<EOF
  > (lang dune 3.12)
  > EOF

We neeed an ocaml file and a dune rule to trigger the logic that would hit ocamlformat

  $ touch dune foo.ml

TODO: Check if ocamlformat is installed and figure out how to remove or mask it if so

  $ ocamlformat

  $ which ocamlformat

  $ dune build @fmt

Combining `(root_module ...)` with a public library results in an unexpected
build failure:

  $ echo "module X = Root.Unix" > main.ml
  $ touch main.opam
  $ cat >dune <<EOF
  > (library
  >  (name main)
  >  (public_name main)
  >  (root_module root)
  >  (libraries unix))
  > EOF

  $ dune build


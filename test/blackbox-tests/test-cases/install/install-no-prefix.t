Here we specify the behaviour of dune install when no install prefix is given.

  $ cat > dune-project << EOF
  > (lang dune 3.9)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (public_name main))
  > EOF

  $ cat > main.ml
  $ cat > proj.opam

  $ dune build @install

We should suggest to the user here that they should use --prefix.

  $ unset OPAM_SWITCH_PREFIX
  $ dune install
  Error: The mandir installation directory is unknown.
  Hint: It can be specified with '--prefix' or by setting '--mandir'
  [1]

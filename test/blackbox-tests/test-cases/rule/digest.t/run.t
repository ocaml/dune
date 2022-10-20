----------------------------------------------------------------------------------
Test that rule digest doesn't depend on irrelevant details of the dune file

  $ export DUNE_PWD_STORE="$(mktemp)"

  $ echo "(lang dune 3.0)" > dune-project

  $ cat >dune <<EOF
  > (rule
  >  (target target)
  >  (mode promote)
  >  (deps (sandbox always) pwd.ml)
  >  (action (run ocaml pwd.ml)))
  > EOF

  $ dune build target
  runnning...
  digest: $1

Let's add a comment to the dune file. It shouldn't affect the rule digest.

  $ cat >dune <<EOF
  > ; hello
  > (rule
  >  (target target)
  >  (mode promote)
  >  (deps (sandbox always) pwd.ml)
  >  (action (run ocaml pwd.ml)))
  > EOF

... and it doesn't.

  $ rm _build/default/target target
  $ dune build @default
  runnning...
  digest: $1

Now the same but with an alias.

  $ cat >dune <<EOF
  > (rule
  >  (alias default)
  >  (deps (sandbox always) pwd.ml)
  >  (action (run ocaml pwd.ml)))
  > EOF

  $ dune build @default
  runnning...
  digest: $2

Let's add a comment to the dune file. One might think that it doesn't affect
the rule digest, but it does because all the locations gets shifted.

  $ cat >dune <<EOF
  > ; hello
  > (rule
  >  (alias default)
  >  (deps (sandbox always) pwd.ml)
  >  (action (run ocaml pwd.ml)))
  > EOF

... but it does! It would be nice to encode the locations in a way that would
make them more resilient to non semantic changes.

# CR-someday amokhov: Remove actual digests from this test so that we don't
# need to update it when rule digest version changes.

  $ dune build @default
  runnning...
  digest: $3

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
  running...
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
  running...
  digest: $1

Now the same but with an alias.

  $ cat >dune <<EOF
  > (rule
  >  (alias default)
  >  (deps (sandbox always) pwd.ml)
  >  (action (run ocaml pwd.ml)))
  > EOF

  $ dune build @default
  running...
  digest: $2

Let's add a comment to the dune file. Dune does not re-run the rule because it
has the same digest:

  $ changelocation() {
  > sed -i.bak '1s/^/;; spurious location change\n/' dune
  > }

  $ changelocation

Now we make sure that failed rules re-run when the location changes:

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (system "echo failing; exit 1")))
  > EOF

  $ dune build @foo
  File "dune", lines 1-3, characters 0-61:
  1 | (rule
  2 |  (alias foo)
  3 |  (action (system "echo failing; exit 1")))
  failing
  [1]

  $ changelocation

This should re-run the action

  $ dune build @foo
  File "dune", lines 2-4, characters 0-61:
  2 | (rule
  3 |  (alias foo)
  4 |  (action (system "echo failing; exit 1")))
  failing
  [1]

  $ dune build @default

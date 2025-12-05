Show PPX snippet preview is shown in Dune

  $ mkdir -p lib
  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (using melange 0.1)
  > EOF
  $ cat > lib/dune <<EOF
  > (library
  >  (name the_lib)
  >  (modes melange)
  >  (preprocess (pps melange.ppx)))
  > EOF
  $ cat > lib/the_lib.ml <<EOF
  > let x: nope = "hello"
  > EOF

  $ export DUNE_SANDBOX=symlink
  $ dune build @all
  File "lib/the_lib.ml", line 1, characters 7-11:
  1 | let x: nope = "hello"
             ^^^^
  Error: Unbound type constructor nope
  [1]

Works if the sandbox is disabled

  $ export DUNE_SANDBOX=none
  $ dune build @all
  File "lib/the_lib.ml", line 1, characters 7-11:
  1 | let x: nope = "hello"
             ^^^^
  Error: Unbound type constructor nope
  [1]

  $ cat > lib/the_lib.mli <<EOF
  > val x: nope
  > EOF

  $ export DUNE_SANDBOX=symlink
  $ dune build @all
  File "lib/the_lib.mli", line 1, characters 7-11:
  1 | val x: nope
             ^^^^
  Error: Unbound type constructor nope
  [1]

  $ export DUNE_SANDBOX=none
  $ dune build @all
  File "lib/the_lib.mli", line 1, characters 7-11:
  1 | val x: nope
             ^^^^
  Error: Unbound type constructor nope
  [1]


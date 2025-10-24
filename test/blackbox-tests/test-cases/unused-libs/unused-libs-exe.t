Test unused library detection in executables

The unused-libs alias should detect unused libraries in executables just like
it does for libraries.

  $ cat > dune-project <<EOF
  > (lang dune 3.21)
  > EOF

Create two libraries - one that will be used and one that won't:

  $ cat > dune <<EOF
  > (library
  >  (name used_lib)
  >  (modules used_lib))
  > 
  > (library
  >  (name unused_lib)
  >  (modules unused_lib))
  > 
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries used_lib unused_lib))
  > EOF

  $ cat > used_lib.ml <<EOF
  > let helper x = x + 1
  > EOF

  $ cat > unused_lib.ml <<EOF
  > let other x = x * 2
  > EOF

  $ cat > main.ml <<EOF
  > (* Only use used_lib, not unused_lib *)
  > let () = print_int (Used_lib.helper 42)
  > EOF

Build the unused-libs alias:

  $ dune build @unused-libs
  File "dune", lines 9-12, characters 0-74:
   9 | (executable
  10 |  (name main)
  11 |  (modules main)
  12 |  (libraries used_lib unused_lib))
  Error: Unused libraries:
  - unused_lib
  [1]

The executable correctly detects unused_lib as unused. The two "All libraries
are used." messages come from the used_lib and unused_lib libraries themselves,
which have no dependencies so they show all their (zero) dependencies as used.

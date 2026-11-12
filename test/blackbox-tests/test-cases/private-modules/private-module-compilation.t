Private wrapper aliases need the private module's CMI in the sandbox.

  $ make_dune_project 3.0
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name lib)
  >  (private_modules priv))
  > EOF
  $ echo 'let value = 1' > lib/priv.ml
  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries lib))
  > EOF
  $ echo 'let _ = Lib.Priv.value' > main.ml

The alias resolves both without sandboxing and in a fresh copy sandbox:

  $ dune build --sandbox none --build-dir _build-none main.bc
  $ dune build --sandbox copy --cache=disabled --build-dir _build-copy main.bc

The private module's internal name remains unavailable:

  $ echo 'let _ = Lib__Priv.value' > main.ml
  $ dune build --sandbox copy --cache=disabled --build-dir _build-copy main.bc
  File "main.ml", line 1, characters 8-17:
  1 | let _ = Lib__Priv.value
              ^^^^^^^^^
  Error: Unbound module Lib__Priv
  [1]

Removing [private_modules] also makes the sandboxed build succeed:

  $ echo '(library (name lib))' > lib/dune
  $ echo 'let _ = Lib.Priv.value' > main.ml
  $ dune build --sandbox copy --cache=disabled --build-dir _build-copy main.bc

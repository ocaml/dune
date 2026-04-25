A consumer that writes `Mylib__Internal.x` — using a wrapped
library's mangled internal module name directly, bypassing the
wrapper — no longer compiles. This file asserts the compile
error.

Why it fails: per-module rule-dep filtering (#4572 / #14116)
scopes each consumer's compile-rule deps to the libraries its
source actually references. The consumer's ocamldep output
names `Mylib__Internal` as a top-level module, but
`Mylib__Internal` is not a library entry — the lib index only
knows about `Mylib`. The filter therefore does not list
`Mylib__Internal.cmi` among the consumer's compile-rule deps;
dune does not order the producing rule before the consumer's
compile, and ocamlc reports `Unbound module` because the file
is absent from `mylib`'s objdir when the consumer's compile
runs.

Why this isn't a regression: the mangled form was never
officially supported. Wrapped libraries expose their public API
through the wrapper module (`Mylib.Internal.x`); the on-disk
mangling to `Mylib__Internal.cmi` is an implementation detail.
Under broad cctx-wide compile-rule deps the leak previously
compiled by accident; per-module rule-dep filtering removes the
accidental reachability. Downstream consumers depending on the
mangled form should migrate to the wrapper API.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ mkdir mylib
  $ cat > mylib/dune <<EOF
  > (library (name mylib))
  > EOF
  $ cat > mylib/mylib.ml <<EOF
  > module Internal = Internal
  > EOF
  $ cat > mylib/internal.ml <<EOF
  > let hi = "hi"
  > EOF

  $ cat > dune <<EOF
  > (executable (name main) (libraries mylib))
  > EOF
  $ cat > main.ml <<EOF
  > let () = print_endline Mylib__Internal.hi
  > EOF

  $ dune build ./main.exe
  File "main.ml", line 1, characters 23-38:
  1 | let () = print_endline Mylib__Internal.hi
                             ^^^^^^^^^^^^^^^
  Error: Unbound module Mylib__Internal
  [1]

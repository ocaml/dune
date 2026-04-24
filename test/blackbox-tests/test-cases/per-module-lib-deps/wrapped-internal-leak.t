Baseline: a consumer can currently reach a wrapped library's internal
(mangled) module name through the include path, because the library's
object directory contains the mangled `Mylib__Internal.cmi` and broad
`-I` flags make that directory reachable.

A wrapped library exposes its public API only through its wrapper
module (`Mylib.Internal`, etc). Dune mangles the on-disk cmi of the
wrapped internal module to `Mylib__Internal.cmi` as an implementation
detail. Under broad per-module `-I` flags, a consumer that writes
`Mylib__Internal.x` compiles successfully, side-stepping the wrapper.

This test records that current behaviour. When per-module `-I`
filtering lands, the consumer module's `-I` paths are scoped to the
libraries it references; `Mylib__Internal` is not an entry in the lib
index, so the filter does not include `mylib` for this consumer, the
objdir is not on `-I`, and the compile is expected to fail with an
"Unbound module Mylib__Internal" error.

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

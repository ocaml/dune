Observational baseline for an unsupported leak: a consumer can
currently reach a wrapped library's internal (mangled) module name
through the include path, because the library's object directory
contains the mangled `Mylib__Internal.cmi` and broad `-I` flags make
that directory reachable.

A wrapped library exposes its public API only through its wrapper
module (`Mylib.Internal`, etc). Dune mangles the on-disk cmi of the
wrapped internal module to `Mylib__Internal.cmi` as an implementation
detail. Under broad per-module `-I` flags, a consumer that writes
`Mylib__Internal.x` compiles successfully, side-stepping the wrapper.

This is not officially supported — consumers should reach a wrapped
library's internals only through the wrapper API, not through the
mangled form (see #14317 review discussion). The leak works today
only as an implementation-detail side effect of the wrapping
convention. In practice some packages currently depend on it, which
is why the baseline is recorded here so any flip is visible in CI.

Per-module `-I` filtering (#4572 / #14186) is expected to break this
leak by scoping each consumer's `-I` paths to the libraries its
source references. `Mylib__Internal` is not an entry in the lib
index, so the filter excludes `mylib`, the objdir is dropped from
`-I`, and the compile fails with "Unbound module Mylib__Internal".
The flip is acceptable because the leak was never supported;
downstream consumers depending on the mangled form should migrate
to the wrapper API.

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

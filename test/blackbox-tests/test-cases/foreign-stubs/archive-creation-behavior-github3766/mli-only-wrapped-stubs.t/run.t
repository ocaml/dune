Test archive creation behavior (ocaml 4.12+): mli-only module, wrapped library, with C stubs.
Expect both foo.a and libfoo_stubs.a to be installed.

  $ dune build @install
  $ ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/foo.a
  _build/install/default/lib/foo/libfoo_stubs.a

Test that the library is usable locally:

  $ dune exec ./exe/b.exe
  exe working

Test that the library is usable externally (without the source):

  $ rm -rf lib
  $ OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working

Test archive creation behavior (ocaml 4.12+): mli-only module, unwrapped library, without C stubs.
Expect no .a files to be installed.

  $ dune build @install
  $ ls _build/install/default/lib/foo/*.a 2>&1 | sed -e 's,^ls:.*foo/\*\.a.*No such file or directory.*$,ls: .../foo/*.a: No such file or directory,g'
  ls: .../foo/*.a: No such file or directory

Test that the library is usable locally:

  $ dune exec ./exe/b.exe
  exe working

Test that the library is usable externally (without the source):

  $ rm -rf lib
  $ OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working

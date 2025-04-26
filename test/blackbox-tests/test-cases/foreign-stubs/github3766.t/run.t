This exercises the difference in behavior when creating archives introduced in
ocaml 4.12.

In 4.11 - .a are always created
In 4.12 - they are omitted if there are no modules or stubs

There's a combination of 3 options:
- With or without stubs
- Wrapped or unwrapped
- Contains an mli only module

Our test checks each of the above with an internal and external library.

  $ ./test.exe |
  > sed -e 's,^ls:.*foo/\*\.a.*No such file or directory.*$,ls: .../foo/*.a: No such file or directory,g' |
  > sed -e 's,^\[[1-2]\]$,[1/2],g'
  # mli_only_wrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  -> creating foo.mli
  % cat dune
  
  (library
   (public_name foo)
   (wrapped true)
   (foreign_stubs (language c) (names stub))
   (modules ()))
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/foo.a
  _build/install/default/lib/foo/libfoo_stubs.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
  
  
  # mli_only_wrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating foo.mli
  % cat dune
  
  (library
   (public_name foo)
   (wrapped true)
   
   (modules ()))
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/foo.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
  
  
  # mli_only_unwrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  -> creating foo.mli
  % cat dune
  
  (library
   (public_name foo)
   (wrapped false)
   (foreign_stubs (language c) (names stub))
   (modules ()))
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/libfoo_stubs.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
  
  
  # mli_only_unwrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating foo.mli
  % cat dune
  
  (library
   (public_name foo)
   (wrapped false)
   
   (modules ()))
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  ls: .../foo/*.a: No such file or directory
  [1/2]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
  
  
  # no_mli_wrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  % cat dune
  
  (library
   (public_name foo)
   (wrapped true)
   (foreign_stubs (language c) (names stub))
   (modules ()))
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/foo.a
  _build/install/default/lib/foo/libfoo_stubs.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
  
  
  # no_mli_wrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  % cat dune
  
  (library
   (public_name foo)
   (wrapped true)
   
   (modules ()))
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/foo.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
  
  
  # no_mli_unwrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  % cat dune
  
  (library
   (public_name foo)
   (wrapped false)
   (foreign_stubs (language c) (names stub))
   (modules ()))
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/libfoo_stubs.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
  
  
  # no_mli_unwrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  % cat dune
  
  (library
   (public_name foo)
   (wrapped false)
   
   (modules ()))
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  ls: .../foo/*.a: No such file or directory
  [1/2]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  exe working
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  exe working
  
  
  mli_only_wrapped_stubs - external - pass
  mli_only_wrapped_stubs - internal - pass
  mli_only_wrapped_no_stubs - external - pass
  mli_only_wrapped_no_stubs - internal - pass
  mli_only_unwrapped_stubs - external - pass
  mli_only_unwrapped_stubs - internal - pass
  mli_only_unwrapped_no_stubs - external - pass
  mli_only_unwrapped_no_stubs - internal - pass
  no_mli_wrapped_stubs - external - pass
  no_mli_wrapped_stubs - internal - pass
  no_mli_wrapped_no_stubs - external - pass
  no_mli_wrapped_no_stubs - internal - pass
  no_mli_unwrapped_stubs - external - pass
  no_mli_unwrapped_stubs - internal - pass
  no_mli_unwrapped_no_stubs - external - pass
  no_mli_unwrapped_no_stubs - internal - pass

This exercises the difference in behavior when creating archives introduced in
ocaml 4.12.

In 4.11 - .a are always created
In 4.12 - they are omitted if there are no modules or stubs

There's a combination of 3 options:
- With or without stubs
- Wrapped or unwrapped
- Contains an mli only module

Our test checks each of the above with an internal and external library.

  $ ./test.exe
  # mli_only_wrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  -> creating foo.mli
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/libfoo_stubs.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: No rule found for lib/foo.a
  [1]
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: File unavailable:
  $TESTCASE_ROOT/mli_only_wrapped_stubs/_build/install/default/lib/foo/foo.a
  [1]
  
  
  # mli_only_wrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating foo.mli
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: No rule found for lib/foo.a
  [1]
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: File unavailable:
  $TESTCASE_ROOT/mli_only_wrapped_no_stubs/_build/install/default/lib/foo/foo.a
  [1]
  
  
  # mli_only_unwrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  -> creating foo.mli
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/libfoo_stubs.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: No rule found for lib/foo.a
  [1]
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: File unavailable:
  $TESTCASE_ROOT/mli_only_unwrapped_stubs/_build/install/default/lib/foo/foo.a
  [1]
  
  
  # mli_only_unwrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating foo.mli
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: No rule found for lib/foo.a
  [1]
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: File unavailable:
  $TESTCASE_ROOT/mli_only_unwrapped_no_stubs/_build/install/default/lib/foo/foo.a
  [1]
  
  
  # no_mli_wrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/libfoo_stubs.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: No rule found for lib/foo.a
  [1]
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: File unavailable:
  $TESTCASE_ROOT/no_mli_wrapped_stubs/_build/install/default/lib/foo/foo.a
  [1]
  
  
  # no_mli_wrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: No rule found for lib/foo.a
  [1]
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: File unavailable:
  $TESTCASE_ROOT/no_mli_wrapped_no_stubs/_build/install/default/lib/foo/foo.a
  [1]
  
  
  # no_mli_unwrapped_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  -> creating stub.c
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  _build/install/default/lib/foo/libfoo_stubs.a
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: No rule found for lib/foo.a
  [1]
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: File unavailable:
  $TESTCASE_ROOT/no_mli_unwrapped_stubs/_build/install/default/lib/foo/foo.a
  [1]
  
  
  # no_mli_unwrapped_no_stubs
  -> creating dune-project
  # build the library and see if .a is present
  -> creating dune
  % dune build --root . @install
  % ls _build/install/default/lib/foo/*.a
  ls: _build/install/default/lib/foo/*.a: No such file or directory
  [1]
  
  # create a dummy executable to test
  -> creating dune
  -> creating b.ml
  
  # make sure that this library is usable locally
  % dune exec ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: No rule found for lib/foo.a
  [1]
  
  # make sure that this library is usable externally
  % rm -rf lib
  % OCAMLPATH=_build/install/default/lib dune exec --build-dir=_b2 ./exe/b.exe
  File "exe/dune", line 3, characters 7-8:
  3 |  (name b)
             ^
  Error: File unavailable:
  $TESTCASE_ROOT/no_mli_unwrapped_no_stubs/_build/install/default/lib/foo/foo.a
  [1]
  
  
  mli_only_wrapped_stubs - external - fail
  mli_only_wrapped_stubs - internal - fail
  mli_only_wrapped_no_stubs - external - fail
  mli_only_wrapped_no_stubs - internal - fail
  mli_only_unwrapped_stubs - external - fail
  mli_only_unwrapped_stubs - internal - fail
  mli_only_unwrapped_no_stubs - external - fail
  mli_only_unwrapped_no_stubs - internal - fail
  no_mli_wrapped_stubs - external - fail
  no_mli_wrapped_stubs - internal - fail
  no_mli_wrapped_no_stubs - external - fail
  no_mli_wrapped_no_stubs - internal - fail
  no_mli_unwrapped_stubs - external - fail
  no_mli_unwrapped_stubs - internal - fail
  no_mli_unwrapped_no_stubs - external - fail
  no_mli_unwrapped_no_stubs - internal - fail

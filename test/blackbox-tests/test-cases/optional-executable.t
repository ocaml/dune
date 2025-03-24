Test optional executable
========================

  $ cat >dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries does-not-exist)
  >  (optional))
  > 
  > (rule
  >  (alias run-x)
  >  (action (run %{exe:x.exe})))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 2.0)
  > (package (name x))
  > EOF

  $ touch x.ml

  $ dune build @install

  $ dune build @all
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist)
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  -> required by _build/default/.x.eobjs/byte/dune__exe__X.cmi
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by alias all
  [1]

  $ dune build @run-x
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist)
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  -> required by _build/default/.x.eobjs/byte/dune__exe__X.cmi
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by %{exe:x.exe} at dune:8
  -> required by alias run-x in dune:6
  [1]

Reproduction case for a bug in dune < 2.4 where all executables where
considered as optional:

  $ cat >dune <<EOF
  > (executable
  >  (public_name x)
  >  (libraries does-not-exist))
  > EOF

The following command should fail because the executable is not optional:

  $ dune build @install
  File "dune", line 3, characters 12-26:
  3 |  (libraries does-not-exist))
                  ^^^^^^^^^^^^^^
  Error: Library "does-not-exist" not found.
  -> required by _build/default/.x.eobjs/byte/dune__exe__X.cmi
  -> required by _build/default/.x.eobjs/native/dune__exe__X.cmx
  -> required by _build/default/x.exe
  -> required by _build/install/default/bin/x
  -> required by _build/default/x.install
  -> required by alias install
  [1]

A strange behavior discovered in #4786. Dune would ignore an executable if any
of its dependencies were optional.

  $ mkdir optional-binary
  $ cd optional-binary
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (package (name myfoo))
  > EOF

  $ mkdir exe
  $ cat >exe/bar.ml <<EOF
  > print_endline "hello world"
  > EOF
  $ cat >exe/dune <<EOF
  > (executable
  >  (public_name dunetestbar)
  >  (name bar)
  >  (libraries foo))
  > EOF

  $ mkdir lib
  $ cat >lib/dune <<EOF
  > (library
  >  (name foo)
  >  (libraries xxx-does-not-exist)
  >  (optional)
  >  (modules ()))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias run-x)
  >  (action (echo %{exe:bar.exe})))
  > EOF

  $ dune build @run-x
  Error: No rule found for bar.exe
  -> required by %{exe:bar.exe} at dune:3
  -> required by alias run-x in dune:1
  [1]

  $ cd ..

When an optional binary is absent, the parent binary should be present. This is
consistent with how libraries work. #4786 notes that this sort of shadowing is
present even if the binary is not optional.

  $ mkdir optional-binary-absent
  $ cd optional-binary-absent
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (package (name myfoo))
  > EOF

  $ mkdir exe
  $ cat >exe/bar.ml <<EOF
  > print_endline "hello world"
  > EOF
  $ cat >exe/dune <<EOF
  > (executable
  >  (public_name dunetestbar)
  >  (libraries doesnotexistatall)
  >  (name bar))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias run-x)
  >  (action (echo "binary path: %{bin:dunetestbar}")))
  > EOF

  $ mkdir bin
  $ cat >bin/dunetestbar <<EOF
  > #!/usr/bin/env bash
  > echo shadow
  > EOF
  $ chmod +x ./bin/dunetestbar

  $ PATH=./bin:$PATH dune build @run-x
  File "exe/dune", line 3, characters 12-29:
  3 |  (libraries doesnotexistatall)
                  ^^^^^^^^^^^^^^^^^
  Error: Library "doesnotexistatall" not found.
  -> required by _build/default/exe/.bar.eobjs/byte/dune__exe__Bar.cmi
  -> required by _build/default/exe/.bar.eobjs/native/dune__exe__Bar.cmx
  -> required by _build/default/exe/bar.exe
  -> required by _build/install/default/bin/dunetestbar
  -> required by %{bin:dunetestbar} at dune:3
  -> required by alias run-x in dune:1
  [1]

Optional on the executable should be respected:

  $ cat >exe/dune <<EOF
  > (executable
  >  (public_name dunetestbar)
  >  (libraries does-not-exist)
  >  (optional)
  >  (name bar))
  > EOF

  $ PATH=./bin:$PATH dune build @run-x
  binary path: $TESTCASE_ROOT/optional-binary-absent/./bin/dunetestbar

In the same way as enabled_if:

  $ cat >exe/dune <<EOF
  > (executable
  >  (public_name dunetestbar)
  >  (enabled_if false)
  >  (name bar))
  > EOF

  $ PATH=./bin:$PATH dune build @run-x --force
  binary path: $TESTCASE_ROOT/optional-binary-absent/./bin/dunetestbar

  $ cd ..


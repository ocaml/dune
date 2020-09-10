Dune prevents overlapping dependencies. These are local dependencies whose names
collide with dependencies already installed. This was introduced in
https://github.com/ocaml/dune/pull/587

First we prepare two external libraries where some_package1 depends on some_package2

  $ mkdir external
  $ cd external
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (package (name some_package1))
  > (package (name some_package2))
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name lib1)
  >  (modules ())
  >  (libraries some_package2)
  >  (public_name some_package1))
  > (library
  >  (name lib2)
  >  (modules ())
  >  (public_name some_package2))
  > EOF
  $ dune build @install
  $ cd ..

Then we create a workspace with some_package2

  $ mkdir use
  $ cd use

  $ mkdir proj1
  $ cd proj1
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (package (name some_package2))
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name lib1)
  >  (modules ())
  >  (public_name some_package2))
  > EOF
  $ cd ..

Then we try to build a library that uses some_package1 which in turn
depends on the overalpping some_package2:
  $ mkdir proj2
  $ cd proj2
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > EOF
  $ cat >dune <<EOF
  > (library
  >  (name bar)
  >  (libraries some_package1))
  > EOF
  $ touch bar.ml
  $ cd ..

And we see the error:
  $ OCAMLPATH=$PWD/../external/_build/install/default/lib dune build %{cma:proj2/bar} --root .
  Error: Conflict between the following libraries:
  - "some_package2" in _build/default/proj1
  - "some_package2" in
    $TESTCASE_ROOT/use/../external/_build/install/default/lib/some_package2
    -> required by library "some_package1" in
       $TESTCASE_ROOT/use/../external/_build/install/default/lib/some_package1
  [1]

We can fix the error by allow overlapping dependencies:
  $ cat >proj2/dune <<EOF
  > (library
  >  (name bar)
  >  (allow_overlapping_dependencies)
  >  (libraries some_package1))
  > EOF
  $ OCAMLPATH=$PWD/../external/_build/install/default/lib dune build %{cma:proj2/bar} --root .

Strangely, the error dissapears if we remove the source for the bar lib:

  $ cat >proj2/dune <<EOF
  > (library
  >  (name bar)
  >  (libraries some_package1))
  > EOF
  $ rm proj2/bar.ml
  $ OCAMLPATH=$PWD/../external/_build/install/default/lib dune build %{cma:proj2/bar} --root .

We also make sure the error exists for executables:
  $ cat >proj2/dune <<EOF
  > (executable
  >  (name bar)
  >  (libraries some_package1))
  > EOF
  $ touch proj2/bar.ml
  $ OCAMLPATH=$PWD/../external/_build/install/default/lib dune build ./proj2/bar.exe --root .
  Error: Conflict between the following libraries:
  - "some_package2" in _build/default/proj1
  - "some_package2" in
    $TESTCASE_ROOT/use/../external/_build/install/default/lib/some_package2
    -> required by library "some_package1" in
       $TESTCASE_ROOT/use/../external/_build/install/default/lib/some_package1
  -> required by executable bar in proj2/dune:2
  [1]

----------------------------------------------------------------------------------
Testsuite for the %{lib...} and %{lib-private...} variable.

  $ make_sandboxed_dune

----------------------------------------------------------------------------------
* Find a public library using the %{lib:...} variable

  $ make_dune_project 2.0
  $ cp -RL lib-pform-fixtures/src .
  $ cat >src/dune <<'EOF'
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib:public_lib:a.ml}")))
  > EOF

  $ touch public_lib.opam
  $ ./sdune build @find-a
  ../install/default/lib/public_lib/a.ml

----------------------------------------------------------------------------------
* Error when finding a public library by its private name using the %{lib:...} variable

  $ cat >dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib:private_lib:a.ml}")))
  > EOF

  $ ./sdune build @find-a
  File "dune", line 3, characters 16-39:
  3 |  (action (echo "%{lib:private_lib:a.ml}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^
  Error: The library "private_lib" is not public. The variable "lib" expands to
  the file's installation path which is not defined for private libraries.
  [1]

----------------------------------------------------------------------------------
* Find a public library by its private name using the %{lib-private:...} variable
* Error when using Dune language < 2.1

  $ cat >dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:private_lib:a.ml}")))
  > EOF

  $ ./sdune build @find-a
  File "dune", line 3, characters 16-47:
  3 |  (action (echo "%{lib-private:private_lib:a.ml}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{lib-private:..} is only available since version 2.1 of the dune
  language. Please update your dune-project file to have (lang dune 2.1).
  [1]

----------------------------------------------------------------------------------
* Find a public library by its private name using the %{lib-private:...} variable
* Success when using Dune language 2.1

  $ make_dune_project 2.1

  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* Find a private library using the %{lib-private:...} variable

  $ cat >src/dune <<'EOF'
  > (library
  >  (name private_lib)
  >  (modules a))
  > EOF

  $ ./sdune clean
  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* The %{lib-private:...} variable works with public library names too

  $ cat >src/dune <<'EOF'
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:public_lib:a.ml}")))
  > EOF

  $ ./sdune clean
  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* The %{lib-private:...} variable does not work with external libraries

  $ (cd lib-pform-fixtures/external && ../../sdune build @install \
  >   && ../../sdune install --prefix install --display quiet)

  $ cat >src/dune-project <<'EOF'
  > (lang dune 2.1)
  > (name test-lib)
  > EOF
  $ cat >src/dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:external_library:opam}")))
  > EOF

  $ OCAMLPATH="$PWD/lib-pform-fixtures/external/install/lib" \
  >   ./sdune build @find-a --root=src
  Entering directory 'src'
  File "dune", line 3, characters 16-52:
  3 |  (action (echo "%{lib-private:external_library:opam}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "lib-private" can only refer to libraries within the same
  project. The current project's name is "test-lib", but the reference is to an
  external library.
  Leaving directory 'src'
  [1]

----------------------------------------------------------------------------------
* The %{lib-private:...} is only allowed within the same project

  $ mkdir -p another
  $ cat >another/dune <<EOF
  > (library
  >  (name anotherlib)
  >  (public_name another_library))
  > EOF

  $ cat >another/dune-project <<'EOF'
  > (lang dune 2.1)
  > (name another-lib)
  > EOF
  $ touch another/another_library.opam

  $ cat >src/dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:another_library:file}")))
  > EOF
  $ rm dune public_lib.opam

  $ ./sdune build @find-a
  File "src/dune", line 3, characters 16-51:
  3 |  (action (echo "%{lib-private:another_library:file}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "lib-private" can only refer to libraries within the same
  project. The current project's name is "test-lib", but the reference is to
  "another-lib".
  [1]

----------------------------------------------------------------------------------
* lib-private with --only-packages
In this test, two packages are defined in the same project, but we may not
access the artifacts through %{lib-private}

  $ make_and_enter_private_only_packages_project lib-private

The build works in development:
  $ dune build @install

But will fail when we release it, as it will need to run with -p:
  $ dune build @install --only-packages public_lib2
  File "lib2/dune", line 5, characters 42-69:
  5 |  (with-stdout-to lib2.ml (echo "let _ = {|%{lib-private:lib1:lib1.ml}|}")))
                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "lib1" not found.
  -> required by %{lib-private:lib1:lib1.ml} at lib2/dune:5
  -> required by _build/default/lib2/lib2.ml
  -> required by _build/install/default/lib/public_lib2/lib2.ml
  -> required by _build/default/public_lib2.install
  -> required by alias install
  [1]

----------------------------------------------------------------------------------
* Error when the wrong number of arguments are passed to %{lib-private:...}

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:a.ml}")))
  > EOF

  $ dune build @find-a
  File "dune", line 3, characters 30-34:
  3 |  (action (echo "%{lib-private:a.ml}")))
                                    ^^^^
  Incorrect arguments for macro lib-private.
  Error: Expected two arguments separated by ':' but no ':' found.
  [1]

----------------------------------------------------------------------------------
Testsuite for the %{lib...} and %{lib-private...} variable.

  $ make_sandboxed_dune

----------------------------------------------------------------------------------
* Find a public library using the %{lib:...} variable

  $ make_dune_project 2.0
  $ mkdir -p src
  $ write_lib_pform_library default public
  $ write_lib_pform_rules lib:public_lib:a.ml default >dune

  $ cat >src/a.ml <<EOF
  > let a = "A"
  > EOF
  $ cat >src/a.mli <<EOF
  > val a : string
  > EOF

  $ touch public_lib.opam
  $ ./sdune build @find-a
  ../install/default/lib/public_lib/a.ml

----------------------------------------------------------------------------------
* Error when finding a public library by its private name using the %{lib:...} variable

  $ write_lib_pform_library default public
  $ write_lib_pform_rules lib:private_lib:a.ml default >dune

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

  $ write_lib_pform_library default public
  $ write_lib_pform_rules lib-private:private_lib:a.ml default >dune

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

  $ write_lib_pform_library default private

  $ ./sdune clean
  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* The %{lib-private:...} variable works with public library names too

  $ write_lib_pform_library default public
  $ write_lib_pform_rules lib-private:public_lib:a.ml default >dune

  $ ./sdune clean
  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* The %{lib-private:...} variable does not work with external libraries

  $ make_lib_pform_external_library 2.1
  $ ( cd external && ../sdune build @install \
  > && ../sdune install --prefix install --display short | dune_cmd sanitize)
  Installing install/lib/external_library/META
  Installing install/lib/external_library/dune-package
  Installing install/lib/external_library/extlib.a
  Installing install/lib/external_library/extlib.cma
  Installing install/lib/external_library/extlib.cmi
  Installing install/lib/external_library/extlib.cmt
  Installing install/lib/external_library/extlib.cmx
  Installing install/lib/external_library/extlib.cmxa
  Installing install/lib/external_library/extlib.ml
  Installing install/lib/external_library/opam
  Installing install/lib/external_library/extlib.cmxs

  $ write_lib_pform_library default public
  $ write_lib_pform_rules lib-private:external_library:opam default >>src/dune

  $ touch src/public_lib.opam
  $ echo "(lang dune 2.1)" > src/dune-project
  $ echo "(name test-lib)" >> src/dune-project

  $ export OCAMLPATH=$PWD/external/install/lib; ./sdune build @find-a --root=src
  Entering directory 'src'
  File "dune", line 7, characters 16-52:
  7 |  (action (echo "%{lib-private:external_library:opam}")))
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

  $ write_lib_pform_library default public
  $ write_lib_pform_rules lib-private:another_library:file default >>src/dune

  $ touch another/another_library.opam
  $ rm public_lib.opam
  $ rm dune
  $ echo "(lang dune 2.1)" > another/dune-project
  $ echo "(name another-lib)" >> another/dune-project

  $ ./sdune build @find-a
  File "src/dune", line 7, characters 16-51:
  7 |  (action (echo "%{lib-private:another_library:file}")))
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

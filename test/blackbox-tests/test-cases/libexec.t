----------------------------------------------------------------------------------
Testsuite for the %{libexec...} and %{libexec-private...} variable.

  $ make_sandboxed_dune
  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context (default (name host)))
  > (context (default
  >  (name target)
  >  (host host)))
  > EOF

----------------------------------------------------------------------------------
* Find a host-context public library using the %{libexec:...} variable

  $ make_dune_project 2.8
  $ mkdir -p src
  $ write_lib_pform_library host public
  $ write_lib_pform_rules libexec:public_lib:a.ml host target >dune

  $ cat >src/a.ml <<EOF
  > let a = "A"
  > EOF
  $ cat >src/a.mli <<EOF
  > val a : string
  > EOF

  $ touch public_lib.opam
  $ ./sdune build @find-a-from-host
  ../install/host/lib/public_lib/a.ml

  $ ./sdune build @find-a-from-target
  ../install/host/lib/public_lib/a.ml

----------------------------------------------------------------------------------
* Error when finding a host-context public library by its private name using the %{libexec:...} variable

  $ write_lib_pform_library host public
  $ write_lib_pform_rules libexec:private_lib:a.ml host target >dune

  $ ./sdune build @find-a-from-host
  File "dune", line 4, characters 16-43:
  4 |  (action (echo "%{libexec:private_lib:a.ml}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The library "private_lib" is not public. The variable "libexec"
  expands to the file's installation path which is not defined for private
  libraries.
  [1]

  $ ./sdune build @find-a-from-target
  File "dune", line 8, characters 16-43:
  8 |  (action (echo "%{libexec:private_lib:a.ml}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The library "private_lib" is not public. The variable "libexec"
  expands to the file's installation path which is not defined for private
  libraries.
  [1]

----------------------------------------------------------------------------------
* Find a host-context private library using the %{libexec-private:...} variable

  $ write_lib_pform_library host private
  $ write_lib_pform_rules libexec-private:private_lib:a.ml host target >dune

  $ ./sdune clean
  $ ./sdune build @find-a-from-host
  src/a.ml
  $ ./sdune build @find-a-from-target
  ../host/src/a.ml

----------------------------------------------------------------------------------
* The %{libexec-private:...} variable works with public library names too

  $ write_lib_pform_library host public
  $ write_lib_pform_rules libexec-private:public_lib:a.ml host target >dune

  $ ./sdune clean
  $ ./sdune build @find-a-from-host
  src/a.ml
  $ ./sdune build @find-a-from-target
  ../host/src/a.ml

----------------------------------------------------------------------------------
* The %{libexec-private:...} variable does not work with external libraries

  $ make_lib_pform_external_library 2.8
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
  $ write_lib_pform_rules libexec-private:external_library:opam \
  >   host target >>src/dune

  $ touch src/public_lib.opam
  $ echo "(lang dune 2.8)" > src/dune-project
  $ echo "(name test-lib)" >> src/dune-project

  $ export OCAMLPATH=$PWD/external/install/lib; ./sdune build @find-a-from-host --root=src --workspace=./dune-workspace
  Entering directory 'src'
  File "dune", line 8, characters 16-56:
  8 |  (action (echo "%{libexec-private:external_library:opam}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "libexec-private" can only refer to libraries within the
  same project. The current project's name is "test-lib", but the reference is
  to an external library.
  Leaving directory 'src'
  [1]

  $ export OCAMLPATH=$PWD/external/install/lib; ./sdune build @find-a-from-target --root=src --workspace=./dune-workspace
  Entering directory 'src'
  File "dune", line 12, characters 16-56:
  12 |  (action (echo "%{libexec-private:external_library:opam}")))
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "libexec-private" can only refer to libraries within the
  same project. The current project's name is "test-lib", but the reference is
  to an external library.
  Leaving directory 'src'
  [1]

----------------------------------------------------------------------------------
* The %{libexec-private:...} is only allowed within the same project

  $ mkdir -p another
  $ cat >another/dune <<EOF
  > (library
  >  (name anotherlib)
  >  (enabled_if (= %{context_name} host))
  >  (public_name another_library))
  > EOF

  $ write_lib_pform_library host public
  $ write_lib_pform_rules libexec-private:another_library:file \
  >   host target >>src/dune

  $ touch another/another_library.opam
  $ rm public_lib.opam
  $ rm dune
  $ echo "(lang dune 2.8)" > another/dune-project
  $ echo "(name another-lib)" >> another/dune-project

  $ ./sdune build @find-a-from-host
  File "src/dune", line 9, characters 16-55:
  9 |  (action (echo "%{libexec-private:another_library:file}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "libexec-private" can only refer to libraries within the
  same project. The current project's name is "test-lib", but the reference is
  to "another-lib".
  [1]

  $ ./sdune build @find-a-from-target
  File "src/dune", line 13, characters 16-55:
  13 |  (action (echo "%{libexec-private:another_library:file}")))
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "libexec-private" can only refer to libraries within the
  same project. The current project's name is "test-lib", but the reference is
  to "another-lib".
  [1]

----------------------------------------------------------------------------------
* libexec-private with --only-packages
In this test, two packages are defined in the same project, but we may not
access the artifacts through %{libexec-private}

  $ make_and_enter_private_only_packages_project \
  >   libexec-private \
  >   "(enabled_if (= %{context_name} host))"
  $ cat >dune <<EOF
  > (alias
  >  (name host)
  >  (enabled_if (= %{context_name} host))
  >  (deps (alias_rec install)))
  > (alias
  >  (name target)
  >  (enabled_if (= %{context_name} target))
  >  (deps (alias_rec install)))
  > EOF

The build works in development:
  $ dune build @host --workspace=../dune-workspace

But will fail when we release it, as it will need to run with -p:
  $ dune build @target --workspace=../dune-workspace --only-packages public_lib2
  File "lib2/dune", line 5, characters 42-73:
  5 |  (with-stdout-to lib2.ml (echo "let _ = {|%{libexec-private:lib1:lib1.ml}|}")))
                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Library "lib1" not found.
  -> required by %{libexec-private:lib1:lib1.ml} at lib2/dune:5
  -> required by _build/target/lib2/lib2.ml
  -> required by _build/install/target/lib/public_lib2/lib2.ml
  -> required by _build/target/public_lib2.install
  -> required by alias install (context target)
  -> required by alias target (context target) in dune:5
  [1]

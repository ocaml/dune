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
  $ cp -RL lib-pform-fixtures/src .
  $ cat >src/dune <<'EOF'
  > (library
  >  (name private_lib)
  >  (enabled_if (= %{context_name} host))
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{libexec:public_lib:a.ml}")))
  > EOF

  $ touch public_lib.opam
  $ ./sdune build @_build/host/find-a
  ../install/host/lib/public_lib/a.ml

  $ ./sdune build @_build/target/find-a
  ../install/host/lib/public_lib/a.ml

----------------------------------------------------------------------------------
* Error when finding a host-context public library by its private name using the %{libexec:...} variable

  $ cat >dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{libexec:private_lib:a.ml}")))
  > EOF

  $ ./sdune build @_build/host/find-a
  File "dune", line 3, characters 16-43:
  3 |  (action (echo "%{libexec:private_lib:a.ml}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The library "private_lib" is not public. The variable "libexec"
  expands to the file's installation path which is not defined for private
  libraries.
  [1]

  $ ./sdune build @_build/target/find-a
  File "dune", line 3, characters 16-43:
  3 |  (action (echo "%{libexec:private_lib:a.ml}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The library "private_lib" is not public. The variable "libexec"
  expands to the file's installation path which is not defined for private
  libraries.
  [1]

----------------------------------------------------------------------------------
* Find a host-context private library using the %{libexec-private:...} variable

  $ cat >src/dune <<'EOF'
  > (library
  >  (name private_lib)
  >  (enabled_if (= %{context_name} host))
  >  (modules a))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{libexec-private:private_lib:a.ml}")))
  > EOF

  $ ./sdune clean
  $ ./sdune build @_build/host/find-a
  src/a.ml
  $ ./sdune build @_build/target/find-a
  ../host/src/a.ml

----------------------------------------------------------------------------------
* The %{libexec-private:...} variable works with public library names too

  $ cat >src/dune <<'EOF'
  > (library
  >  (name private_lib)
  >  (enabled_if (= %{context_name} host))
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{libexec-private:public_lib:a.ml}")))
  > EOF

  $ ./sdune clean
  $ ./sdune build @_build/host/find-a
  src/a.ml
  $ ./sdune build @_build/target/find-a
  ../host/src/a.ml

----------------------------------------------------------------------------------
* The %{libexec-private:...} variable does not work with external libraries

  $ (cd lib-pform-fixtures/external && ../../sdune build @install \
  >   && ../../sdune install --prefix install --display quiet)

  $ cat >src/dune-project <<'EOF'
  > (lang dune 2.8)
  > (name test-lib)
  > EOF
  $ cat >src/dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{libexec-private:external_library:opam}")))
  > EOF

  $ OCAMLPATH="$PWD/lib-pform-fixtures/external/install/lib" \
  >   ./sdune build @_build/host/find-a --root=src --workspace=./dune-workspace
  Entering directory 'src'
  File "dune", line 3, characters 16-56:
  3 |  (action (echo "%{libexec-private:external_library:opam}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "libexec-private" can only refer to libraries within the
  same project. The current project's name is "test-lib", but the reference is
  to an external library.
  Leaving directory 'src'
  [1]

  $ OCAMLPATH="$PWD/lib-pform-fixtures/external/install/lib" \
  >   ./sdune build @_build/target/find-a --root=src --workspace=./dune-workspace
  Entering directory 'src'
  File "dune", line 3, characters 16-56:
  3 |  (action (echo "%{libexec-private:external_library:opam}")))
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

  $ cat >another/dune-project <<'EOF'
  > (lang dune 2.8)
  > (name another-lib)
  > EOF
  $ touch another/another_library.opam

  $ cat >src/dune <<'EOF'
  > (rule
  >  (alias find-a)
  >  (action (echo "%{libexec-private:another_library:file}")))
  > EOF
  $ rm dune public_lib.opam

  $ ./sdune build @_build/host/find-a
  File "src/dune", line 3, characters 16-55:
  3 |  (action (echo "%{libexec-private:another_library:file}")))
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: The variable "libexec-private" can only refer to libraries within the
  same project. The current project's name is "test-lib", but the reference is
  to "another-lib".
  [1]

  $ ./sdune build @_build/target/find-a
  File "src/dune", line 3, characters 16-55:
  3 |  (action (echo "%{libexec-private:another_library:file}")))
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

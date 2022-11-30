----------------------------------------------------------------------------------
Testsuite for the %{libexec...} and %{libexec-private...} variable.

  $ cat >sdune <<'EOF'
  > #!/usr/bin/env bash
  > DUNE_SANDBOX=symlink dune "$@"
  > EOF
  $ chmod +x sdune
  $ cat >dune-workspace <<EOF
  > (lang dune 2.0)
  > (context (default (name host)))
  > (context (default
  >  (name target)
  >  (host host)))
  > EOF

----------------------------------------------------------------------------------
* Find a host-context public library using the %{libexec:...} variable

  $ echo "(lang dune 2.8)" > dune-project
  $ mkdir -p src
  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (enabled_if (= %{context_name} host))
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a-from-host)
  >  (enabled_if (= %{context_name} host))
  >  (action (echo "%{libexec:public_lib:a.ml}")))
  > (rule
  >  (alias find-a-from-target)
  >  (enabled_if (= %{context_name} target))
  >  (action (echo "%{libexec:public_lib:a.ml}")))
  > EOF

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

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (enabled_if (= %{context_name} host))
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a-from-host)
  >  (enabled_if (= %{context_name} host))
  >  (action (echo "%{libexec:private_lib:a.ml}")))
  > (rule
  >  (alias find-a-from-target)
  >  (enabled_if (= %{context_name} target))
  >  (action (echo "%{libexec:private_lib:a.ml}")))
  > EOF

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

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (enabled_if (= %{context_name} host))
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a-from-host)
  >  (enabled_if (= %{context_name} host))
  >  (action (echo "%{libexec-private:private_lib:a.ml}")))
  > (rule
  >  (alias find-a-from-target)
  >  (enabled_if (= %{context_name} target))
  >  (action (echo "%{libexec-private:private_lib:a.ml}")))
  > EOF

  $ ./sdune clean
  $ ./sdune build @find-a-from-host
  src/a.ml
  $ ./sdune build @find-a-from-target
  ../host/src/a.ml

----------------------------------------------------------------------------------
* The %{libexec-private:...} variable works with public library names too

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (enabled_if (= %{context_name} host))
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a-from-host)
  >  (enabled_if (= %{context_name} host))
  >  (action (echo "%{libexec-private:public_lib:a.ml}")))
  > (rule
  >  (alias find-a-from-target)
  >  (enabled_if (= %{context_name} target))
  >  (action (echo "%{libexec-private:public_lib:a.ml}")))
  > EOF

  $ ./sdune clean
  $ ./sdune build @find-a-from-host
  src/a.ml
  $ ./sdune build @find-a-from-target
  ../host/src/a.ml

----------------------------------------------------------------------------------
* The %{libexec-private:...} variable does not work with external libraries

  $ mkdir -p external
  $ cat >external/dune-project <<EOF
  > (lang dune 2.8)
  > (name external_library)
  > EOF
  $ cat >external/dune <<EOF
  > (library
  >  (name extlib)
  >  (public_name external_library))
  > EOF

  $ touch external/external_library.opam
  $ ( cd external && ../sdune build @install && ../sdune install --prefix install | dune_cmd sanitize)
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

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > (rule
  >  (alias find-a-from-host)
  >  (enabled_if (= %{context_name} host))
  >  (action (echo "%{libexec-private:external_library:opam}")))
  > (rule
  >  (alias find-a-from-target)
  >  (enabled_if (= %{context_name} target))
  >  (action (echo "%{libexec-private:external_library:opam}")))
  > EOF

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

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (enabled_if (= %{context_name} host))
  >  (public_name public_lib)
  >  (modules a))
  > (rule
  >  (alias find-a-from-host)
  >  (enabled_if (= %{context_name} host))
  >  (action (echo "%{libexec-private:another_library:file}")))
  > (rule
  >  (alias find-a-from-target)
  >  (enabled_if (= %{context_name} target))
  >  (action (echo "%{libexec-private:another_library:file}")))
  > EOF

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

  $ mkdir lib-private-only-packages
  $ cd lib-private-only-packages
  $ mkdir lib1 lib2
  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (name lib-private-test)
  > (package (name public_lib1))
  > (package (name public_lib2))
  > EOF
  $ cat >lib1/dune <<EOF
  > (library
  >  (name lib1)
  >  (enabled_if (= %{context_name} host))
  >  (public_name public_lib1))
  > EOF
  $ touch lib1/lib1.ml
  $ cat >lib2/dune <<EOF
  > (library
  >  (name lib2)
  >  (public_name public_lib2))
  > (rule
  >  (with-stdout-to lib2.ml (echo "let _ = {|%{libexec-private:lib1:lib1.ml}|}")))
  > EOF
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
  -> required by _build/target/lib2/.lib2.objs/byte/lib2.cmi
  -> required by _build/target/lib2/.lib2.objs/native/lib2.cmx
  -> required by _build/target/lib2/lib2.a
  -> required by _build/install/target/lib/public_lib2/lib2.a
  -> required by _build/target/public_lib2.install
  -> required by alias install (context target)
  -> required by alias target (context target) in dune:5
  [1]

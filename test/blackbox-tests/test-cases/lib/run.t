----------------------------------------------------------------------------------
Testsuite for the %{lib...} and %{lib-private...} variable.
TODO: Fix %{libexec} and %{libexec-private} variables and test them.

  $ cat >sdune <<'EOF'
  > #!/usr/bin/env bash
  > DUNE_SANDBOX=symlink dune "$@"
  > EOF
  $ chmod +x sdune

----------------------------------------------------------------------------------
* Find a public library using the %{lib:...} variable

  $ echo "(lang dune 2.0)" > dune-project
  $ mkdir -p src
  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib:public_lib:a.ml}")))
  > EOF

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

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib:private_lib:a.ml}")))
  > EOF

  $ ./sdune build @find-a
  File "dune", line 3, characters 18-39:
  3 |  (action (echo "%{lib:private_lib:a.ml}")))
                        ^^^^^^^^^^^^^^^^^^^^^
  Error: The library "private_lib" is not public. The variable "lib" expands to
  the file's installation path which is not defined for private libraries.
  [1]

----------------------------------------------------------------------------------
* Find a public library by its private name using the %{lib-private:...} variable
* Error when using Dune language < 2.1

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:private_lib:a.ml}")))
  > EOF

  $ ./sdune build @find-a
  File "dune", line 3, characters 18-47:
  3 |  (action (echo "%{lib-private:private_lib:a.ml}")))
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: %{lib-private:..} is only available since version 2.1 of the dune
  language. Please update your dune-project file to have (lang 2.1).
  [1]

----------------------------------------------------------------------------------
* Find a public library by its private name using the %{lib-private:...} variable
* Success when using Dune language 2.1

  $ echo "(lang dune 2.1)" > dune-project

  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* Find a private library using the %{lib-private:...} variable

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (modules a))
  > EOF

  $ ./sdune clean
  $ ./sdune build @find-a
  src/a.ml

----------------------------------------------------------------------------------
* The %{lib-private:...} variable works with public library names too

  $ cat >src/dune <<EOF
  > (library
  >  (name private_lib)
  >  (public_name public_lib)
  >  (modules a))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias find-a)
  >  (action (echo "%{lib-private:public_lib:a.ml}")))
  > EOF

  $ ./sdune clean
  $ ./sdune build @find-a
  src/a.ml

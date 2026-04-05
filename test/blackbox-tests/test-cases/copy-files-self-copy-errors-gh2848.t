----------------------------------------------------------------------------------
Testsuite for https://github.com/ocaml/dune/issues/2848
(copy_files ...) cannot copy files onto themselves. The format for the argument
is <dir>/<glob> where <dir> is not the current directory.

  $ cat >sdune <<'EOF'
  > #!/usr/bin/env bash
  > DUNE_SANDBOX=symlink dune "$@"
  > EOF
  $ chmod +x sdune

----------------------------------------------------------------------------------
* Good error message when <dir> is the current directory

  $ echo "(lang dune 2.2)" > dune-project

  $ cat >dune <<EOF
  > (executable (name foo))
  > (copy_files sub)
  > EOF

  $ cat >foo.ml <<EOF
  > let () = Bar.bar ()
  > EOF

  $ mkdir -p sub

  $ cat >sub/bar.ml <<EOF
  > let bar () = print_endline "Hello, world!"
  > EOF

  $ ./sdune build
  File "dune", line 2, characters 12-15:
  2 | (copy_files sub)
                  ^^^
  Error: Cannot copy files onto themselves. The format is <dir>/<glob> where
  <dir> is not the current directory.
  [1]

----------------------------------------------------------------------------------
* Good error message when <dir> is missing

  $ echo "(lang dune 2.2)" > dune-project

  $ cat >dune <<EOF
  > (executable (name foo))
  > (copy_files bar.{ml})
  > EOF

  $ ./sdune build
  File "dune", line 2, characters 12-20:
  2 | (copy_files bar.{ml})
                  ^^^^^^^^
  Error: Cannot copy files onto themselves. The format is <dir>/<glob> where
  <dir> is not the current directory.
  [1]

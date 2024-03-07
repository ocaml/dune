Build a package with make
  $ . ./helpers.sh

  $ mkdir foo
  $ cat >foo/Makefile <<EOF
  > .DEFAULT: foo
  > .PHONY: foo
  > foo: ; @echo running makefile
  > EOF
  $ make_lockdir
  $ cat >dune.lock/foo.pkg <<EOF
  > (version 0.0.1)
  > (build (run %{make}))
  > (source (copy $PWD/foo))
  > EOF
  $ build_pkg foo
  running makefile

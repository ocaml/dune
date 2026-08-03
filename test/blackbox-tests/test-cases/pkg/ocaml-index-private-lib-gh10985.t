This test attempts to build the ocaml index while depending on a library
installed through a lock file.

We set up a library that will be installed as part of the package:

  $ make_external_mypkg_lib_source 'let x = ()'

We put the actual build in a separate directory, so we don't have to ignore
the package directory in the dune file:
  $ mkdir actual
  $ cd actual

Now we set up a lock file with this package and then attempt to use it:

  $ make_dune_project 3.11

  $ make_lockdir
  $ make_lockpkg mypkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/../external_sources))
  > (build (run dune build --release --promote-install-file=true . @install))
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (libraries mypkg.lib))
  > EOF

  $ cat >foo.ml <<EOF
  > let () = Test_lib.x
  > EOF

  $ mkdir .bin
  $ cat > .bin/ocaml-index <<EOF
  > #!/usr/bin/env sh
  > exit 1
  > EOF

  $ chmod +x .bin/ocaml-index
  $ export PATH="$PWD/.bin:$PATH"

  $ dune build @ocaml-index
  File ".foo.objs/_unknown_", line 1, characters 0-0:
  Command exited with code 1.
  [1]

This test attempts to build a library installed through a lock file and then
use it inside dune.

We set up a library that will be installed as part of the package:

  $ make_external_mypkg_lib_source 'let x = ()'

Now we set up a lock file with this package and then attempt to use it:

  $ make_dune_project 3.11

  $ make_lockdir
  $ make_lockpkg mypkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/external_sources))
  > (build (run dune build --release --promote-install-file=true . @install))
  > EOF

  $ cat >dune <<EOF
  > (dirs (:standard \ external_sources))
  > (library
  >  (name foo)
  >  (libraries mypkg.lib))
  > EOF

  $ cat >foo.ml <<EOF
  > let () = Test_lib.x
  > EOF

  $ dune build foo.cma

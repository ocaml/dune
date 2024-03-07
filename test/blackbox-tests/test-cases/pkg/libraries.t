This test attempts to build a library installed through a lock file and then
use it inside dune.

  $ . ./helpers.sh

We set up a library that will be installed as part of the package:

  $ mkdir external_sources
  $ cat >external_sources/dune-project <<EOF
  > (lang dune 3.11)
  > (package (name mypkg))
  > EOF
  $ cat >external_sources/dune <<EOF
  > (library
  >  (public_name mypkg.lib)
  >  (name test_lib))
  > EOF
  $ cat >external_sources/test_lib.ml <<EOF
  > let x = ()
  > EOF

Now we set up a lock file with this package and then attempt to use it:

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF

  $ cat >dune.lock/mypkg.pkg <<EOF
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

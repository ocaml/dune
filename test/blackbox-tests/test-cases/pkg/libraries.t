This test attempts to build a library installed through a lock file and then
use it inside dune.

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
  > (source (copy $PWD/extra_sources))
  > (build (run dune build @install))
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
  File "dune", line 4, characters 12-21:
  4 |  (libraries mypkg.lib))
                  ^^^^^^^^^
  Error: Library "mypkg.lib" not found.
  -> required by library "foo" in _build/default
  -> required by _build/default/.foo.objs/byte/foo.cmo
  -> required by _build/default/foo.cma
  [1]

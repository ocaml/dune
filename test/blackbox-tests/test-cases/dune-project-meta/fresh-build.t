Building the .install file should not depend on foo.opam being present in the
source tree if (generate_opam_files true) is enabled.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name foo))
  > (generate_opam_files true)
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (section share)
  >  (files dune))
  > EOF

  $ dune build foo.install
  $ grep opam _build/default/foo.install
    "_build/install/default/lib/foo/opam"

  $ dune build @check
  $ dune build foo.install
  $ grep opam _build/default/foo.install
    "_build/install/default/lib/foo/opam"

The same should hold once opam generation switches to diff actions.

  $ rm -f foo.opam
  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (package
  >  (name foo))
  > (generate_opam_files true)
  > EOF

  $ dune build foo.install
  $ grep opam _build/default/foo.install
    "_build/install/default/lib/foo/opam"
  $ if test -e foo.opam; then echo present; else echo absent; fi
  present

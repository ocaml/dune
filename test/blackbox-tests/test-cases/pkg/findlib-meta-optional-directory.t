Demonstrate the handling of findlib directories that don't exist

Reproduces #11405

  $ mkdir external_sources

  $ cat >external_sources/META <<EOF
  > package "yes" (
  >   directory = "yes"
  >   version = "0.0.1"
  >   exists_if = "yes.cma"
  > )
  > package "no" (
  >   directory = "no"
  >   version = "0.0.1"
  >   exists_if = "no.cma"
  > )
  > EOF

  $ cat >external_sources/mypkg.install <<EOF
  > lib: [
  >  "META"
  >  "yes/yes.cma" {"yes/yes.cma"}
  > ]
  > EOF

  $ mkdir external_sources/yes
  $ touch external_sources/yes/yes.cma

  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF

  $ make_lockdir

  $ make_lockpkg mypkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/external_sources))
  > EOF

  $ touch foo.ml

  $ cat >dune <<EOF
  > (executable
  >  (libraries mypkg.yes)
  >  (name foo))
  > EOF

No errors here as 'yes' actually exists
  $ dune build foo.exe

  $ cat >dune <<EOF
  > (executable
  >  (libraries mypkg.no)
  >  (name foo))
  > EOF

Clearer error here as we really depend on non-existing 'no'
  $ dune build foo.exe 2>&1 | sanitize_pkg_digest mypkg.0.0.1
  File "dune", line 2, characters 12-20:
  2 |  (libraries mypkg.no)
                  ^^^^^^^^
  Error: Library "mypkg.no" in
  _build/_private/default/.pkg/mypkg.0.0.1-DIGEST_HASH/target/lib/mypkg/no
  is hidden (unsatisfied 'exists_if').
  -> required by _build/default/.foo.eobjs/native/dune__exe__Foo.cmx
  -> required by _build/default/foo.exe

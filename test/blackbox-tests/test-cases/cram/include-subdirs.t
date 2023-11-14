Cram tests inside (include_subdirs unqualified)

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF

  $ cat >dune <<EOF
  > (include_subdirs unqualified)
  > EOF

  $ mkdir sub/
  $ cat >sub/foo.t <<EOF
  >   $ echo foo
  > EOF

  $ dune runtest

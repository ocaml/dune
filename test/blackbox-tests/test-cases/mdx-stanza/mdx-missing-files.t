  $ cat >dune <<EOF
  > (mdx (files missing.md))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 2.4)
  > (using mdx 0.1)
  > EOF

Dune should complain about `missing.md` not existing and not being possible to
generate it when running the tests:

  $ dune runtest

Test also version 0.2 of the stanza:
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using mdx 0.2)
  > EOF

  $ dune runtest

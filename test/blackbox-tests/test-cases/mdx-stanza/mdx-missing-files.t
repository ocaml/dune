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
  File "dune", line 1, characters 0-24:
  1 | (mdx (files missing.md))
      ^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: The glob used in the 'files' field of this mdx stanza does not match
  any file. Note that in the absence of the 'files' field the default glob
  '*.md' is used.

Test also version 0.2 of the stanza:
  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using mdx 0.2)
  > EOF

  $ dune runtest
  File "dune", line 1, characters 0-24:
  1 | (mdx (files missing.md))
      ^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: The glob used in the 'files' field of this mdx stanza does not match
  any file. Note that in the absence of the 'files' field the default glob
  '*.md' is used.

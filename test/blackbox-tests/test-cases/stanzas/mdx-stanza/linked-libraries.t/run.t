Since 0.2 you can use the `libraries` field to have them linked into the test executable

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using mdx 0.2)
  > (cram disable)
  > EOF

  $ dune runtest

MDX stanza 0.1 does not support linking libraries

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (using mdx 0.1)
  > EOF

  $ dune runtest
  File "dune", line 3, characters 1-24:
  3 |  (libraries private_lib))
       ^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'libraries' is only available since version 0.2 of mdx extension to
  verify code blocks in .md files. Please update your dune-project file to have
  (using mdx 0.2).
  [1]

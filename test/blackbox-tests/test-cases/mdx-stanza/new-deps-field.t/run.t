Since version 0.2 the mdx stanza supports a more generic `deps` field.
A deprecation warning is raised if using the olf `packages` field.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using mdx 0.2)
  > (cram disable)
  > EOF

  $ dune runtest
  File "dune", line 4, characters 1-15:
  4 |  (packages pkg))
       ^^^^^^^^^^^^^^
  Warning: 'packages' was deprecated in version 0.2 of mdx extension to verify
  code blocks in .md files.

But using the new field with the old stanza would fail

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (using mdx 0.1)
  > EOF

  $ dune runtest
  File "dune", line 3, characters 1-22:
  3 |  (deps (package pkg2))
       ^^^^^^^^^^^^^^^^^^^^^
  Error: 'deps' is only available since version 0.2 of mdx extension to verify
  code blocks in .md files. Please update your dune-project file to have (using
  mdx 0.2).
  [1]

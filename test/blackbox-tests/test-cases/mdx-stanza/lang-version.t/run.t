The MDX stanza requires dune lang 2.4 or higher

  $ cat >dune-project <<EOF
  > (lang dune 2.3)
  > (using mdx 0.1)
  > EOF

  $ dune build @install
  File "dune-project", line 2, characters 11-14:
  2 | (using mdx 0.1)
                 ^^^
  Warning: Version 0.1 of mdx extension to verify code blocks in .md files is
  not supported until version 2.4 of the dune language.
  There are no supported versions of this extension in version 2.3 of the dune
  language.
  
  File "dune", line 1, characters 0-5:
  1 | (mdx)
      ^^^^^
  Error: 'mdx' is only available since version 2.4 of the dune language. Please
  update your dune-project file to have (lang dune 2.4).
  [1]

The version 0.2 requires dune 3.0

  $ cat >dune-project <<EOF
  > (lang dune 2.9)
  > (using mdx 0.2)
  > (cram disable)
  > EOF
  $ dune build @install
  File "dune-project", line 2, characters 11-14:
  2 | (using mdx 0.2)
                 ^^^
  Error: Version 0.2 of mdx extension to verify code blocks in .md files is not
  supported until version 3.0 of the dune language.
  Supported versions of this extension in version 2.9 of the dune language:
  - 0.1
  [1]

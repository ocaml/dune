To use the mdx stanza you need to explicitly set (using mdx ..) in the
dune-project

  $ dune build @install --root using-mdx/
  Entering directory 'using-mdx'
  Info: Appending this line to dune-project: (using mdx 0.1)

It also requires dune lang 2.4 or higher

  $ dune build @install --root lang-version/
  Entering directory 'lang-version'
  File "dune", line 1, characters 0-5:
  1 | (mdx)
      ^^^^^
  Error: 'mdx' is only available since version 2.4 of the dune language. Please
  update your dune-project file to have (lang 2.4).
  [1]

You can use the mdx stanza to check your documentation in markdown and mli files

  $ dune runtest --root simple/
  Entering directory 'simple'
  File "README.md", line 1, characters 0-0:
  Error: Files _build/default/README.md and _build/default/README.md.corrected
  differ.
  [1]

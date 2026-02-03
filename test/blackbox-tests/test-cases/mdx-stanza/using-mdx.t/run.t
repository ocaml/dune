  $ make_dune_project 2.4

To use the mdx stanza you need to explicitly set (using mdx ..) in the
dune-project

  $ dune build @install
  File "dune", line 1, characters 0-5:
  1 | (mdx)
      ^^^^^
  Error: 'mdx' is available only when mdx is enabled in the dune-project or
  workspace file. You must enable it using (using mdx 0.1) in the file.
  [1]

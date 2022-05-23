Check the error message when using an extension that is not available
at the current language version:
  $ dune build
  File "dune", line 1, characters 0-5:
  1 | (mdx)
      ^^^^^
  Error: 'mdx' is available only when mdx is enabled in the dune-project file.
  You must enable it using (using mdx ..) in your dune-project file.
  Note however that the currently selected version of dune (1.0) does not
  support this plugin. The first version of this plugin is 0.1 and was
  introduced in dune 2.4.
  [1]

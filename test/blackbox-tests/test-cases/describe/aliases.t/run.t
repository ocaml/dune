Testing the "dune show aliases" command. This command shows the aliases in the
current directory. It acts similarly to ls. It will not show aliases that appear
in subdirectories although this could be changed in the future.

In an empty dune project, the following aliases are available.

  $ dune show aliases
  all
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt
  ocaml-index
  pkg-install

User defined aliases can be added to a dune file. These should be picked up by
the command.

  $ cat > dune << EOF
  > (alias
  >  (name foo))
  > EOF

  $ dune show aliases
  all
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt
  foo
  ocaml-index
  pkg-install

Aliases in subdirectories should not be picked up.

  $ mkdir subdir
  $ cat > subdir/dune << EOF
  > (alias
  >  (name bar))
  > EOF

  $ dune show aliases
  all
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt
  foo
  ocaml-index
  pkg-install

But checking the subdirectory it should be available.

  $ dune show aliases subdir
  all
  bar
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt

Adding an OCaml library will introduce OCaml specific aliases:

  $ cat > dune << EOF
  > (library
  >  (name foo))
  > EOF

  $ dune show aliases
  all
  check
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  doc-private
  fmt
  ocaml-index
  pkg-install

Adding a cram test will introduce an alias with the name of the test and also
introduce the runtest alias:
bbb
  $ rm dune
  $ cat > mytest.t

  $ dune show aliases
  all
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt
  mytest
  ocaml-index
  pkg-install
  runtest

We can also show aliases in multiple directories at once:

  $ dune show aliases . subdir
  .:
  all
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt
  mytest
  ocaml-index
  pkg-install
  runtest
  
  subdir:
  all
  bar
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt

Including those in the _build/ directory:

  $ dune build
  $ dune show aliases . _build/default
  .:
  all
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt
  mytest
  ocaml-index
  pkg-install
  runtest
  
  _build/default:
  all
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt
  mytest
  ocaml-index
  pkg-install
  runtest

These are context sensitive:

  $ cat > dune-workspace << EOF
  > (lang dune 3.9)
  > (context
  >  (default
  >   (name other_context)))
  > EOF

  $ dune show aliases --context other_context _build/default
  Error: Directory _build/default is not in context "other_context".

  $ dune show aliases --context other_context _build/other_context
  all
  default
    - This alias corresponds to the default argument for dune build. More
      information on
      https://dune.readthedocs.io/en/latest/reference/aliases/default.html
  fmt
  mytest
  ocaml-index
  pkg-install
  runtest

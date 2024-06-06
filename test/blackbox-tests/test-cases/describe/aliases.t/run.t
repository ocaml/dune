Testing the "dune show aliases" command. This command shows the aliases in the
current directory. It acts similarly to ls. It will not show aliases that appear
in subdirectories although this could be changed in the future.

In an empty dune project, the following aliases are available.

  $ dune show aliases
  all
  default
  fmt
  ocaml-index

User defined aliases can be added to a dune file. These should be picked up by
the command.

  $ cat > dune << EOF
  > (alias
  >  (name foo))
  > EOF

  $ dune show aliases
  all
  default
  fmt
  foo
  ocaml-index

Aliases in subdirectories should not be picked up.

  $ mkdir subdir
  $ cat > subdir/dune << EOF
  > (alias
  >  (name bar))
  > EOF

  $ dune show aliases
  all
  default
  fmt
  foo
  ocaml-index

But checking the subdirectory it should be available.

  $ dune show aliases subdir
  all
  bar
  default
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
  doc-private
  fmt
  ocaml-index

Adding a cram test will introduce an alias with the name of the test and also
introduce the runtest alias:
bbb
  $ rm dune
  $ cat > mytest.t

  $ dune show aliases
  all
  default
  fmt
  mytest
  ocaml-index
  runtest

We can also show aliases in multiple directories at once:

  $ dune show aliases . subdir
  .:
  all
  default
  fmt
  mytest
  ocaml-index
  runtest
  
  subdir:
  all
  bar
  default
  fmt

Including those in the _build/ directory:

  $ dune build
  $ dune show aliases . _build/default
  .:
  all
  default
  fmt
  mytest
  ocaml-index
  runtest
  
  _build/default:
  all
  default
  fmt
  mytest
  ocaml-index
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
  fmt
  mytest
  ocaml-index
  runtest

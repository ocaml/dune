Testing the "dune show aliases" command. This command shows the aliases in the
current directory. It acts similarly to ls. It will not show aliases that appear
in subdirectories although this could be changed in the future.

In an empty dune project, the following aliases are available.

  $ dune show aliases
  all
  default
  fmt
  ocaml-index
  pkg-install
  revdep
  revdep-check
  revdep-install
  revdep-runtest

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
  pkg-install
  revdep
  revdep-check
  revdep-install
  revdep-runtest

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
  pkg-install
  revdep
  revdep-check
  revdep-install
  revdep-runtest

But checking the subdirectory it should be available.

  $ dune show aliases subdir
  all
  bar
  default
  fmt
  revdep
  revdep-check
  revdep-install
  revdep-runtest

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
  pkg-install
  revdep
  revdep-check
  revdep-install
  revdep-runtest
  unused-libs

At dune 3.25, adding a cram test will introduce an alias with the full name of
the test and also introduce the runtest alias:

  $ rm -f dune-project
  $ make_dune_project 3.25

  $ rm dune
  $ cat > mytest.t

  $ dune show aliases
  all
  default
  fmt
  mytest.t
  ocaml-index
  pkg-install
  revdep
  revdep-check
  revdep-install
  revdep-runtest
  runtest

We can also show aliases in multiple directories at once:

  $ dune show aliases . subdir
  .:
  all
  default
  fmt
  mytest.t
  ocaml-index
  pkg-install
  revdep
  revdep-check
  revdep-install
  revdep-runtest
  runtest
  
  subdir:
  all
  bar
  default
  fmt
  revdep
  revdep-check
  revdep-install
  revdep-runtest

Including those in the _build/ directory:

  $ dune build
  $ dune show aliases . _build/default
  .:
  all
  default
  fmt
  mytest.t
  ocaml-index
  pkg-install
  revdep
  revdep-check
  revdep-install
  revdep-runtest
  runtest
  
  _build/default:
  all
  default
  fmt
  mytest.t
  ocaml-index
  pkg-install
  revdep
  revdep-check
  revdep-install
  revdep-runtest
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
  mytest.t
  pkg-install
  revdep
  revdep-check
  revdep-install
  revdep-runtest
  runtest

Before dune 3.25, cram aliases drop the .t suffix:

  $ rm dune-workspace
  $ make_dune_project 3.24

  $ dune show aliases > aliases-v324 2>&1
  $ grep '^mytest' aliases-v324
  mytest

  $ dune build @mytest

  $ dune test mytest.t

From dune 3.25 onward, cram aliases keep the full test name:

  $ make_dune_project 3.25

  $ dune show aliases > aliases-v325 2>&1
  $ grep '^mytest' aliases-v325
  mytest.t

  $ dune build @mytest.t

  $ dune test mytest.t

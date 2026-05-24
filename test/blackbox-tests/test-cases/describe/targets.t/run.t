Testing the "dune show targets" command in a simple OCaml project with an
additional directory target to see the behaviour there.

We have two libraries with one in a subdirectory. We also have a directory
target d to see how the command will behave.

With no directory provided to the command, it should default to the current
working directory.

  $ dune show targets
  _doc/
  _doc_new/
  a.ml
  b/
  d/
  dune
  dune-project
  simple.a
  simple.cma
  simple.cmxa
  simple.cmxs
  simple.ml-gen

Multiple directories can be provided to the command. Also subdirectories may be
used, and only the targets available in that directory will be displayed.

  $ dune show targets . b/
  .:
  _doc/
  _doc_new/
  a.ml
  b/
  d/
  dune
  dune-project
  simple.a
  simple.cma
  simple.cmxa
  simple.cmxs
  simple.ml-gen
  
  b:
  c.ml
  dune
  simple2.a
  simple2.cma
  simple2.cmxa
  simple2.cmxs
  simple2.ml-gen

The command also works with files in the _build directory.

  $ dune show targets _build/default/
  _doc/
  _doc_new/
  a.ml
  b/
  d/
  dune
  dune-project
  simple.a
  simple.cma
  simple.cmxa
  simple.cmxs
  simple.ml-gen

  $ dune show targets _build/default/b
  c.ml
  dune
  simple2.a
  simple2.cma
  simple2.cmxa
  simple2.cmxs
  simple2.ml-gen

We cannot see inside directory targets

  $ dune show targets d
  Error: Directory d is a directory target. This command does not support the
  inspection of directory targets.

Build-only directories that don't exist in the source tree, like .simple.objs
(Dune's internal object directory for the `simple` library), can now be
queried:

  $ dune show targets .simple.objs
  byte/
  cctx.ocaml-index
  jsoo/
  native/

  $ dune show targets _build/default/.simple.objs
  byte/
  cctx.ocaml-index
  jsoo/
  native/

With --all, hidden directories are also shown:

  $ dune show targets --all .simple.objs
  .bin/
  .utop/
  byte/
  cctx.ocaml-index
  jsoo/
  native/

And we error on non-existent directories

  $ dune show targets non-existent
  Error: Directory non-existent does not exist.

We error if we are in the wrong context

  $ dune show targets _build/other_context
  Error: Directory _build/other_context is not in context "default".

  $ cat > dune-workspace << EOF
  > (lang dune 3.9)
  > (context
  >  (default
  >   (name other_context)))
  > EOF

  $ dune show targets --context other_context _build/default
  Error: Directory _build/default is not in context "other_context".

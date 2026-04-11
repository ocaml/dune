@ocaml-index should not build more than necessary (#12007).

In particular, it should not try to index stanzas in alternative contexts
that are not relevant to the default build.

Setup mock ocaml-index:

  $ mkdir bin
  $ ln -s $(which ocaml_index) bin/ocaml-index
  $ export PATH=$PWD/bin:$PATH

Test 1: @ocaml-index indexes alternative contexts it shouldn't
==============================================================

A project with a default context and an alt context. The main executable
depends on defaultlib which is only enabled in the default context.

@ocaml-index should only index the default context, but instead it
tries to index the alt context too — and fails because defaultlib
is hidden there.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune-workspace <<EOF
  > (lang dune 3.23)
  > (context default)
  > (context
  >  (default
  >   (name alt)))
  > EOF

  $ mkdir -p defaultlib altlib

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries defaultlib))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_endline Defaultlib.greeting
  > EOF

  $ cat > defaultlib/dune <<EOF
  > (library
  >  (name defaultlib)
  >  (enabled_if (= %{context_name} "default")))
  > EOF

  $ cat > defaultlib/defaultlib.ml <<EOF
  > let greeting = "hello from default"
  > EOF

  $ cat > altlib/dune <<EOF
  > (library
  >  (name altlib)
  >  (enabled_if (= %{context_name} "alt")))
  > EOF

  $ cat > altlib/altlib.ml <<EOF
  > let greeting = "hello from alt"
  > EOF

@ocaml-index should only index the default context, not alt:

  $ dune build @ocaml-index

  $ find _build -name '*.ocaml-index' | sort
  _build/default/.main.eobjs/cctx.ocaml-index
  _build/default/defaultlib/.defaultlib.objs/cctx.ocaml-index

  $ rm -rf _build dune dune-project dune-workspace main.ml defaultlib altlib

Test 2: @ocaml-index correctly skips disabled stanzas (not a bug)
=================================================================

Verify that (enabled_if false) libraries are already correctly
excluded from indexing.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (modules main))
  > (library
  >  (name disabled_lib)
  >  (modules disabled_lib)
  >  (enabled_if false))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_endline "hello"
  > EOF

  $ cat > disabled_lib.ml <<EOF
  > let x = 42
  > EOF

  $ dune build @ocaml-index 2>&1

Only main is indexed, disabled_lib is correctly skipped:

  $ find _build -name '*.ocaml-index' | sort
  _build/default/.main.eobjs/cctx.ocaml-index

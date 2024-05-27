 Temporary special merlin support for melange only libs

  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ melc_compiler="$(which melc)"
  $ export BUILD_PATH_PREFIX_MAP="$(melc_stdlib_prefix)":$BUILD_PATH_PREFIX_MAP
  $ export BUILD_PATH_PREFIX_MAP="/MELC_COMPILER=$melc_compiler:$BUILD_PATH_PREFIX_MAP"
  $ export BUILD_PATH_PREFIX_MAP="/MELC_STDLIB=$(ocamlfind query melange):$BUILD_PATH_PREFIX_MAP"

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ lib=foo
  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (modes melange))
  > EOF

  $ touch bar.ml $lib.ml
  $ dune build @check
  $ dune ocaml merlin dump-config "$PWD" | grep -i "$lib"
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/melange)
   (FLG (-open Foo__))
   (UNIT_NAME foo__Bar))
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/melange)
   (FLG (-open Foo__))
   (UNIT_NAME foo__Bar))
  Foo: _build/default/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/melange)
   (FLG (-open Foo__))
   (UNIT_NAME foo))
  Foo: _build/default/foo.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/melange)
   (FLG (-open Foo__))
   (UNIT_NAME foo))
  Foo__: _build/default/foo__
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/melange)
   (UNIT_NAME foo__))
  Foo__: _build/default/foo__.ml-gen
  ((INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (B $TESTCASE_ROOT/_build/default/.foo.objs/melange)
   (UNIT_NAME foo__))

Paths to Melange stdlib appear in B and S entries without melange.emit stanza

  $ dune ocaml dump-dot-merlin $PWD | grep -e "^B " -e "^S "
  B /MELC_STDLIB/__private__/melange_mini_stdlib/melange/.public_cmi_melange
  B /MELC_STDLIB/melange
  B /MELC_STDLIB/melange
  B $TESTCASE_ROOT/_build/default/.foo.objs/melange
  S /MELC_STDLIB
  S /MELC_STDLIB/__private__/melange_mini_stdlib
  S /MELC_STDLIB
  S $TESTCASE_ROOT

  $ target=output
  $ cat >dune <<EOF
  > (melange.emit
  >  (target "$target")
  >  (compile_flags :standard -mel-g )
  >  (emit_stdlib false)
  >  (modules main))
  > EOF

  $ touch main.ml
  $ dune build @check
  $ dune ocaml merlin dump-config $PWD | grep -i "$target"
   (B $TESTCASE_ROOT/_build/default/.output.mobjs/melange)
   (B $TESTCASE_ROOT/_build/default/.output.mobjs/melange)

Dump-dot-merlin includes the melange flags

  $ dune ocaml dump-dot-merlin $PWD
  EXCLUDE_QUERY_DIR
  STDLIB /MELC_STDLIB/melange
  SOURCE_ROOT $TESTCASE_ROOT
  B /MELC_STDLIB/__private__/melange_mini_stdlib/melange/.public_cmi_melange
  B /MELC_STDLIB/melange
  B /MELC_STDLIB/melange
  B $TESTCASE_ROOT/_build/default/.output.mobjs/melange
  S /MELC_STDLIB
  S /MELC_STDLIB/__private__/melange_mini_stdlib
  S /MELC_STDLIB
  S $TESTCASE_ROOT
  # FLG -w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -mel-g
  
Check for flag directives ordering when another preprocessor is defined

  $ cat >fooppx.ml <<EOF
  > open Ppxlib
  > 
  > let rules =
  >   let extension =
  >     Extension.declare "test" Expression Ast_pattern.__ (fun ~loc ~path:_ _ ->
  >       Ast_builder.Default.eint ~loc 42)
  >   in
  >   [ Context_free.Rule.extension extension ]
  > 
  > let () = Ppxlib.Driver.register_transformation "rules" ~rules
  > EOF

  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (modules bar)
  >  (preprocess (pps fooppx))
  >  (modes melange))
  > (library
  >  (name fooppx)
  >  (modules fooppx)
  >  (libraries ppxlib)
  >  (kind ppx_rewriter))
  > EOF

  $ dune build @check

User ppx flags should appear in merlin config

  $ dune ocaml merlin dump-config $PWD | grep -v "(B "  | grep -v "(S "
  Bar: _build/default/bar
  ((INDEX $TESTCASE_ROOT/_build/default/.fooppx.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (STDLIB /MELC_STDLIB/melange)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (FLG (-open Foo))
   (FLG (-ppx "$TESTCASE_ROOT/_build/default/.ppx/4128e43a9cfb141a37f547484cc9bf46/ppx.exe --as-ppx --cookie 'library-name="foo"'"))
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo__Bar))
  Bar: _build/default/bar.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.fooppx.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (STDLIB /MELC_STDLIB/melange)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (FLG (-open Foo))
   (FLG (-ppx "$TESTCASE_ROOT/_build/default/.ppx/4128e43a9cfb141a37f547484cc9bf46/ppx.exe --as-ppx --cookie 'library-name="foo"'"))
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo__Bar))
  Foo: _build/default/foo
  ((INDEX $TESTCASE_ROOT/_build/default/.fooppx.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (STDLIB /MELC_STDLIB/melange)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (FLG (-ppx "$TESTCASE_ROOT/_build/default/.ppx/4128e43a9cfb141a37f547484cc9bf46/ppx.exe --as-ppx --cookie 'library-name="foo"'"))
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo))
  Foo: _build/default/foo.ml-gen
  ((INDEX $TESTCASE_ROOT/_build/default/.fooppx.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (STDLIB /MELC_STDLIB/melange)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (FLG (-ppx "$TESTCASE_ROOT/_build/default/.ppx/4128e43a9cfb141a37f547484cc9bf46/ppx.exe --as-ppx --cookie 'library-name="foo"'"))
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME foo))
  Fooppx: _build/default/fooppx
  ((INDEX $TESTCASE_ROOT/_build/default/.fooppx.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME fooppx))
  Fooppx: _build/default/fooppx.ml
  ((INDEX $TESTCASE_ROOT/_build/default/.fooppx.objs/cctx.ocaml-index)
   (INDEX $TESTCASE_ROOT/_build/default/.foo.objs/cctx.ocaml-index)
   (STDLIB /OCAMLC_WHERE)
   (SOURCE_ROOT $TESTCASE_ROOT)
   (EXCLUDE_QUERY_DIR)
   (FLG (-w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs -g))
   (UNIT_NAME fooppx))

Generate an inferred interface for a module using the module's build
configuration, and register it for promotion.

  $ make_dune_project 3.25
  $ echo '(lang dune 3.25)' >dune-workspace

  $ mkdir dep ppx foo
  $ cat >dep/dune <<'EOF'
  > (library (name dep))
  > EOF
  $ cat >dep/dep.ml <<'EOF'
  > let value = 20
  > EOF

  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF
  $ cat >ppx/ppx.ml <<'EOF'
  > open Ppxlib
  > 
  > let extension =
  >   Extension.declare "inferred_mli" Expression Ast_pattern.__
  >     (fun ~loc ~path:_ _ -> Ast_builder.Default.estring ~loc "generated")
  > 
  > let () =
  >   Driver.register_transformation
  >     "inferred_mli"
  >     ~rules:[ Context_free.Rule.extension extension ]
  > EOF

  $ cat >foo/dune <<'EOF'
  > (library
  >  (name foo)
  >  (libraries dep)
  >  (preprocess (pps ppx)))
  > EOF
  $ cat >foo/helper.ml <<'EOF'
  > let value = 22
  > EOF
  $ cat >foo/foo.ml <<'EOF'
  > let answer = Dep.value + Helper.value
  > let text = [%inferred_mli]
  > EOF

A module without an interface gets one as a correction registered for promotion.
Run the command from the module's directory, with workspace root detection
enabled.

  $ (cd foo &&
  >  unset INSIDE_DUNE &&
  >  dune ocaml inferred-mli --diff-command=- foo.ml)
  Entering directory '$TESTCASE_ROOT'
  File "foo/foo.mli", line 1, characters 0-0:
  Error: Files _build/default/foo/foo.mli and
  _build/default/foo/foo.mli.corrected differ.
  Leaving directory '$TESTCASE_ROOT'
  [1]
  $ test ! -e foo/foo.mli
  $ test ! -e _build/default/foo/foo.mli
  $ dune promotion list
  foo/foo.mli

  $ dune promote foo/foo.mli >/dev/null 2>&1
  $ cat foo/foo.mli
  val answer : int
  val text : string

A module with an up-to-date interface does not register a promotion.

  $ dune ocaml inferred-mli foo/foo.ml
  $ dune promotion list

A module with an outdated interface gets an updated correction.

  $ echo 'val answer : string' >foo/foo.mli
  $ dune ocaml inferred-mli foo/foo.ml
  File "foo/foo.mli", line 1, characters 0-0:
  --- foo/foo.mli
  +++ foo/foo.mli.corrected
  @@ -1 +1,2 @@
  -val answer : string
  +val answer : int
  +val text : string
  [1]
  $ dune promotion list
  foo/foo.mli

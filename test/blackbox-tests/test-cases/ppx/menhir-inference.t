Menhir inference with ordinary and staged PPX exposes shadowed aliases (#8989).

  $ make_menhir_project 3.11 2.1
  $ mkdir -p ppx lib/group
  $ cat >ppx/dune <<'EOF'
  > (library (name packed_ppx) (kind ppx_rewriter) (libraries ppxlib))
  > EOF
  $ cat >ppx/packed_ppx.ml <<'EOF'
  > open Ppxlib
  > let packed =
  >   Extension.V3.declare "packed" Extension.Context.expression
  >     Ast_pattern.(pstr nil)
  >     (fun ~ctxt ->
  >       let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >       Ast_builder.Default.pexp_ident ~loc
  >         { txt = Longident.Ldot (Longident.Lident "M", "packed"); loc })
  > let () =
  >   Driver.register_transformation "packed"
  >     ~rules:[ Context_free.Rule.extension packed ]
  > EOF
  $ echo '(menhir (modules group))' >lib/group/dune
  $ cat >lib/group/m.ml <<'EOF'
  > module type S = sig end
  > let packed = (module struct end : S)
  > EOF
  $ cat >lib/group/group.mly <<'EOF'
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { [%packed] }
  > EOF

Ordinary PPX produces a binary AST; staged PPX runs inside the compiler.

  $ build () {
  >   cat >lib/dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo) (wrapped false) (preprocess ($1 packed_ppx)))
  > EOF
  >   dune build lib/foo.cma 2>errors
  > }
  $ build pps
  [1]
  $ sed -n '/val xv_main/p;/Error: Syntax error/{p;q;}' \
  >   _build/default/lib/group/group__mock.mli.inferred errors
  val xv_main : (module Group__/2.M.S)
  Error: Syntax error
  $ build staged_pps
  [1]
  $ sed -n '/val xv_main/p;/Error: Syntax error/{p;q;}' \
  >   _build/default/lib/group/group__mock.mli.inferred errors
  val xv_main : (module Group__/2.M.S)
  Error: Syntax error

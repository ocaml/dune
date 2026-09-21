Menhir inference with ordinary and staged PPX must avoid shadowed aliases (#8989).
The dependency introduced by PPX also shares its name with the root alias Foo.

  $ make_menhir_project 3.25 3.0
  $ mkdir -p ppx lib/group
  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name packed_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF
  $ cat >ppx/packed_ppx.ml <<'EOF'
  > open Ppxlib
  > let packed =
  >   Extension.V3.declare "packed" Extension.Context.expression
  >     Ast_pattern.(pstr nil)
  >     (fun ~ctxt ->
  >       let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >       Ast_builder.Default.pexp_ident ~loc
  >         { txt = Longident.Ldot (Longident.Lident "Foo", "packed"); loc })
  > let () =
  >   Driver.register_transformation "packed"
  >     ~rules:[ Context_free.Rule.extension packed ]
  > EOF
  $ cat >lib/group/dune <<'EOF'
  > (menhir
  >  (modules group))
  > EOF
  $ cat >lib/group/foo.ml <<'EOF'
  > module type S = sig end
  > let packed = (module struct end : S)
  > EOF
  $ cat >lib/group/group.mly <<'EOF'
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { [%packed] }
  > EOF

  $ build () {
  >   cat >lib/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name foo)
  >  (preprocess ($1 packed_ppx)))
  > EOF
  >   dune build lib/foo.cma
  > }
  $ build pps
  $ build staged_pps

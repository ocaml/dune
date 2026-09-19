Menhir inference must preprocess semantic actions with ordinary PPX, staged
PPX, and action preprocessing. Group interfaces also expose the shadowed alias
problem from #8989 after preprocessing.

  $ make_menhir_project 3.11 2.1
  $ mkdir -p ppx ordinary/group
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
  >         { txt = Longident.Ldot (Longident.Lident "M", "packed"); loc })
  > let () =
  >   Driver.register_transformation "packed"
  >     ~rules:[ Context_free.Rule.extension packed ]
  > EOF
  $ cat >ordinary/dune <<'EOF'
  > (include_subdirs qualified)
  > (executable
  >  (name main)
  >  (preprocess (pps packed_ppx)))
  > EOF
  $ cat >ordinary/main.ml <<'EOF'
  > let () =
  >   Group.Parser.main (fun _ -> Group.Parser.EOF) (Lexing.from_string "")
  > EOF
  $ cat >ordinary/group/dune <<'EOF'
  > (menhir (modules parser))
  > EOF
  $ cat >ordinary/group/m.mli <<'EOF'
  > module type S = sig end
  > val packed : (module S)
  > EOF
  $ cat >ordinary/group/m.ml <<'EOF'
  > module type S = sig end
  > let packed = (module struct end : S)
  > EOF
  $ cat >ordinary/group/parser.mly <<'EOF'
  > %token EOF
  > %start <unit> main
  > %%
  > main: v=value EOF { ignore v }
  > value: { [%packed] }
  > EOF

All three preprocessing modes turn the extension into a value whose inferred
type is a first-class module. Ordinary PPX produces a binary AST, whereas
staged PPX runs inside the compiler.

  $ cp -R ordinary staged
  $ cat >staged/dune <<'EOF'
  > (include_subdirs qualified)
  > (executable
  >  (name main)
  >  (preprocess (staged_pps packed_ppx)))
  > EOF
  $ cp -R ordinary action
  $ cat >action/dune <<'EOF'
  > (include_subdirs qualified)
  > (executable
  >  (name main)
  >  (preprocess
  >   (action
  >    (run sed "s/\\[%packed\\]/M.packed/g" %{input-file}))))
  > EOF
  $ dune exec ordinary/main.exe
  $ dune exec staged/main.exe
  $ dune exec action/main.exe

Inference errors in preprocessed semantic actions should still point to the
original grammar rather than the generated mock source.

  $ sed 's/ignore v/v + 1/' staged/group/parser.mly >ordinary/group/parser.mly
  $ dune build ordinary/group/parser__mock.mli.inferred 2>error
  [1]
  $ sed -n '/^File "ordinary\/group\/parser.mly"/p' error
  File "ordinary/group/parser.mly", line 4, characters 20-21:
  $ cp staged/group/parser.mly ordinary/group/parser.mly

Now make the parser the group interface. The inferred type refers to a hidden
alias, producing invalid OCaml under each preprocessing mode.

  $ for mode in ordinary staged action; do
  >   mv "$mode/group/parser.mly" "$mode/group/group.mly"
  >   echo '(menhir (modules group))' >"$mode/group/dune"
  >   sed 's/Group.Parser/Group/g' "$mode/main.ml" >"$mode/main.ml.new"
  >   mv "$mode/main.ml.new" "$mode/main.ml"
  > done
  $ dune build ordinary/main.exe 2>ordinary-errors
  [1]
  $ sed -n '/val xv_value/p' _build/default/ordinary/group/group__mock.mli.inferred
  val xv_value : (module Dune__exe__Group__/2.M.S)
  $ dune build staged/main.exe 2>staged-errors
  [1]
  $ sed -n '/val xv_value/p' _build/default/staged/group/group__mock.mli.inferred
  val xv_value : (module Dune__exe__Group__/2.M.S)
  $ dune build action/main.exe 2>action-errors
  [1]
  $ sed -n '/val xv_value/p' _build/default/action/group/group__mock.mli.inferred
  val xv_value : (module Dune__exe__Group__/2.M.S)

PPX-generated module references must be discovered in implementations and
interfaces, with ordinary and staged preprocessing for OCaml and Melange.

  $ make_melange_project 3.25 1.0
  $ mkdir ppx
  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name dependency_ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF
  $ cat >ppx/dependency_ppx.ml <<'EOF'
  > open Ppxlib
  > let reference ~loc module_ name =
  >   { txt = Longident.Ldot (Longident.Lident module_, name); loc }
  > let value =
  >   Extension.declare "generated_value" Extension.Context.expression
  >     Ast_pattern.(pstr nil) (fun ~loc ~path:_ ->
  >       Ast_builder.Default.pexp_ident ~loc
  >         (reference ~loc "Z_value" "value"))
  > let type_ =
  >   Extension.declare "generated_type" Extension.Context.core_type
  >     Ast_pattern.(pstr nil) (fun ~loc ~path:_ ->
  >       Ast_builder.Default.ptyp_constr ~loc (reference ~loc "Z_type" "t") [])
  > let () =
  >   Driver.register_transformation "dependencies"
  >     ~rules:(List.map Context_free.Rule.extension [value; type_])
  > EOF
  $ make_case () {
  >   mkdir "$1"
  >   cat >"$1/dune" <<EOF
  > (library
  >  (name $1)
  >  (wrapped false)
  >  (modules shared z_type z_value)
  >  (modes :standard melange)
  >  (preprocess ($2 dependency_ppx)))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries $1))
  > (melange.emit
  >  (target out)
  >  (modules main)
  >  (libraries $1))
  > EOF
  >   echo 'type t = string' >"$1/z_type.ml"
  >   echo 'let value = "generated"' >"$1/z_value.ml"
  >   echo 'let value = [%generated_value]' >"$1/shared.ml"
  >   echo 'val value : [%generated_type]' >"$1/shared.mli"
  >   echo 'let () = print_endline Shared.value' >"$1/main.ml"
  > }
  $ make_case ordinary pps
  $ make_case staged staged_pps

Staged JS emission scans the source for implementation dependencies. Provide
that reference explicitly; the interface dependency still comes only from PPX.

  $ echo 'let () = ignore Z_value.value' >>staged/shared.ml

Build each interface alone first: this must build its generated dependency
Z_type, without building the implementation dependency Z_value. Then building
the implementation must discover Z_value. Separate object directories keep
the OCaml and Melange checks independent.

  $ check_dependencies () {
  >   obj_dir="$1/.$1.objs/$2"
  >   dune build "$obj_dir/shared.cmi" &&
  >   test -f "_build/default/$obj_dir/z_type.cmi" &&
  >   test ! -f "_build/default/$obj_dir/z_value.cmi" &&
  >   dune build "$obj_dir/shared.$3" &&
  >   test -f "_build/default/$obj_dir/z_value.cmi"
  > }
  $ check_dependencies ordinary byte cmo
  $ check_dependencies ordinary melange cmj
  $ check_dependencies staged byte cmo
  $ check_dependencies staged melange cmj

Both executables and emitted JavaScript can use the generated references.

  $ dune build ordinary/main.exe @ordinary/melange \
  >   staged/main.exe @staged/melange
  $ _build/default/ordinary/main.exe
  generated
  $ node _build/default/ordinary/out/ordinary/main.js
  generated
  $ _build/default/staged/main.exe
  generated
  $ node _build/default/staged/out/staged/main.js
  generated

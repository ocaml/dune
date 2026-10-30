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

  $ dune build ordinary/main.exe @ordinary/melange staged/main.exe
  $ _build/default/ordinary/main.exe
  generated
  $ node _build/default/ordinary/out/ordinary/main.js
  generated
  $ _build/default/staged/main.exe
  generated

Staged emission must also discover dependencies introduced by the PPX.

  $ dune build @staged/melange && node _build/default/staged/out/staged/main.js
  generated

Direct emission also preserves staged PPX arguments and generated preprocessor
inputs. The input is declared separately from the flag, so it must be available
in the sandbox through preprocessor_deps.

  $ cat >>ppx/dependency_ppx.ml <<'EOF'
  > let module_file = ref ""
  > let configured_value =
  >   Extension.declare "configured_value" Expression Ast_pattern.(pstr nil)
  >     (fun ~loc ~path:_ ->
  >       if not (Sys.file_exists !module_file) then
  >         Location.raise_errorf ~loc "Missing PPX input";
  >       let input = open_in !module_file in
  >       let selected = input_line input in
  >       close_in input;
  >       Ast_builder.Default.pexp_ident ~loc (reference ~loc selected "value"))
  > let () =
  >   Driver.add_arg "--module-file" (Arg.Set_string module_file)
  >     ~doc:"Read the generated dependency's module name";
  >   Driver.register_transformation "configured_dependencies"
  >     ~rules:[Context_free.Rule.extension configured_value]
  > EOF
  $ mkdir emit
  $ cat >emit/dune <<'EOF'
  > (rule
  >  (target module-name)
  >  (deps selection)
  >  (action (copy %{deps} %{target})))
  > (melange.emit
  >  (target out)
  >  (preprocess
  >   (staged_pps dependency_ppx -- --module-file emit/module-name))
  >  (preprocessor_deps module-name (sandbox always)))
  > EOF
  $ echo Z_first >emit/selection
  $ echo 'let value = "first"' >emit/z_first.ml
  $ echo 'let value = "second"' >emit/z_second.ml
  $ echo 'let () = print_endline [%configured_value]' >emit/main.ml

Request main.js first, before the other JavaScript outputs can build unrelated
modules. Then build the remaining outputs so Node can load the selected module.

  $ dune build emit/out/emit/main.js && dune build @emit/melange \
  >   && node _build/default/emit/out/emit/main.js
  first

Changing only the generated preprocessor input changes the discovered module
reference and the emitted program.

  $ echo Z_second >emit/selection
  $ dune build emit/out/emit/main.js && dune build @emit/melange \
  >   && node _build/default/emit/out/emit/main.js
  second

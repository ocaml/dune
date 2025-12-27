open Import

type 'a args = 'a Command.Args.t list

let ocamllex_bin sctx ~loc ~dir =
  Super_context.resolve_program sctx ~loc:(Some loc) ~dir ~where:Original_path "ocamllex"
;;

let ocamllex sctx ~loc ~dir (args : 'a args) : Action.Full.t Action_builder.With_targets.t
  =
  let ocamllex_bin = ocamllex_bin sctx ~loc ~dir in
  let build_dir = Super_context.context sctx |> Context.build_dir in
  Command.run_dyn_prog (* ~sandbox *) ~dir:(Path.build build_dir) ocamllex_bin args
;;

let rule sctx ~dir ~loc ~mode : Action.Full.t Action_builder.With_targets.t -> unit Memo.t
  =
  Super_context.add_rule sctx ~dir ~mode ~loc
;;

let gen_rules ~sctx ~dir { Ocamllex.loc; modules; mode; enabled_if = _ } =
  let module S = String_with_vars in
  Memo.sequential_iter modules ~f:(fun name ->
    let src = Path.Build.relative dir (name ^ ".mll") in
    let dst = Path.Build.relative dir (name ^ ".ml") in
    let open Memo.O in
    let* mode =
      let* expander = Super_context.expander sctx ~dir in
      Rule_mode_expand.expand_path ~expander ~dir mode
    in
    let action =
      ocamllex
        sctx
        ~loc
        ~dir
        [ As [ "-q"; "-o" ]; Target dst; Command.Args.Dep (Path.build src) ]
    in
    rule sctx ~dir ~loc ~mode action)
;;

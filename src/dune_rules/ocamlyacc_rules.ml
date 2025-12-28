open Import

type 'a args = 'a Command.Args.t list

let ocamlyacc_bin sctx ~loc ~dir =
  Super_context.resolve_program sctx ~loc:(Some loc) ~dir ~where:Original_path "ocamlyacc"
;;

let ocamlyacc sctx ~loc ~dir (args : 'a args)
  : Action.Full.t Action_builder.With_targets.t
  =
  let ocamlyacc_bin = ocamlyacc_bin sctx ~loc ~dir in
  let build_dir = Super_context.context sctx |> Context.build_dir in
  Command.run_dyn_prog (* ~sandbox *) ~dir:(Path.build build_dir) ocamlyacc_bin args
;;

let rule sctx ~dir ~loc ~mode : Action.Full.t Action_builder.With_targets.t -> unit Memo.t
  =
  Super_context.add_rule sctx ~dir ~mode ~loc
;;

let gen_rules ~sctx ~dir { Ocamlyacc.loc; modules; mode; enabled_if = _ } =
  Memo.sequential_iter modules ~f:(fun name ->
    let src = Path.Build.relative dir (name ^ ".mly") in
    let open Memo.O in
    let* mode =
      let* expander = Super_context.expander sctx ~dir in
      Rule_mode_expand.expand_path ~expander ~dir mode
    in
    let targets = List.map [ name ^ ".ml"; name ^ ".mli" ] ~f:(Path.Build.relative dir) in
    let action =
      ocamlyacc
        sctx
        ~loc
        ~dir
        [ Command.Args.Dep (Path.build src); Hidden_targets targets ]
    in
    rule sctx ~dir ~loc ~mode action)
;;

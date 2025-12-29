open Import

type 'a args = 'a Command.Args.t list

let ocamllex =
  let ocamllex_bin sctx ~loc ~dir =
    Super_context.resolve_program
      sctx
      ~loc:(Some loc)
      ~dir
      ~where:Original_path
      "ocamllex"
  in
  fun sctx ~loc ~dir (args : 'a args) : Action.Full.t Action_builder.With_targets.t ->
    let ocamllex_bin = ocamllex_bin sctx ~loc ~dir in
    let build_dir = Super_context.context sctx |> Context.build_dir in
    Command.run_dyn_prog
      ~sandbox:Sandbox_config.needs_sandboxing
      ~dir:(Path.build build_dir)
      ocamllex_bin
      args
;;

let gen_rules ~sctx ~dir_contents ~dir t =
  let open Memo.O in
  let { Ocamllex.loc; mode; modules = _; enabled_if = _ } = t in
  let* ml_sources = Dir_contents.ocaml dir_contents in
  let source_modules =
    Ml_sources.Parser_generators.source_modules ml_sources ~for_:(Ocamllex loc)
  in
  match source_modules with
  | None -> Memo.return ()
  | Some source_modules ->
    Module_trie.to_map source_modules
    |> Module_name.Map.to_list
    |> Memo.parallel_iter ~f:(fun (_name, m) ->
      let source = Module.Source.files m |> List.hd in
      let src_dir = Module.Source.src_dir m |> Path.as_in_build_dir_exn in
      match Path.Build.equal dir src_dir with
      | false ->
        let ocamllex_dir = Path.Build.drop_build_context_exn dir in
        let src_dir = Path.Build.drop_build_context_exn src_dir in
        User_error.raise
          ~loc
          [ Pp.text
              "The `ocamllex' stanza for a module must be specified in the same \
               directory as the module it generates."
          ; Pp.textf "- module directory: %s" (Path.Source.to_string src_dir)
          ; Pp.textf "- ocamllex directory: %s" (Path.Source.to_string ocamllex_dir)
          ]
      | true ->
        let action =
          let src = Module.File.original_path source in
          let dst = Module.File.path source |> Path.as_in_build_dir_exn in
          ocamllex sctx ~loc ~dir [ As [ "-q"; "-o" ]; Target dst; Command.Args.Dep src ]
        in
        let* mode =
          let* expander = Super_context.expander sctx ~dir in
          Rule_mode_expand.expand_path ~expander ~dir mode
        in
        Super_context.add_rule sctx ~dir ~mode ~loc action)
;;

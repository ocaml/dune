open Import

let tool =
  let tool_bin sctx ~loc ~dir ~for_ =
    Super_context.resolve_program
      sctx
      ~loc:(Some loc)
      ~dir
      ~where:Original_path
      (Parser_generators.tool for_)
  in
  fun sctx ~loc ~dir args ~for_ ->
    let tool_bin = tool_bin sctx ~loc ~dir ~for_ in
    let build_dir = Super_context.context sctx |> Context.build_dir |> Path.build in
    Command.run_dyn_prog
      ~sandbox:Sandbox_config.needs_sandboxing
      ~dir:build_dir
      tool_bin
      args
;;

let add_rule sctx ~dir ~loc ~mode m ~for_ =
  let args =
    let files = Module.Source.files m in
    let file = List.hd files in
    let src = Module.File.original_path file in
    match for_ with
    | Parser_generators.Ocamllex _ ->
      let dst = Module.File.path file |> Path.as_in_build_dir_exn in
      [ Command.Args.As [ "-q"; "-o" ]; Target dst; Command.Args.Dep src ]
    | Ocamlyacc _ ->
      let targets =
        List.map files ~f:(fun file -> Module.File.path file |> Path.as_in_build_dir_exn)
      in
      [ Command.Args.Dep src; Hidden_targets targets ]
  in
  let action = tool sctx ~loc ~dir args ~for_ in
  let open Memo.O in
  let* mode =
    let* expander = Super_context.expander sctx ~dir in
    Rule_mode_expand.expand_path ~expander ~dir mode
  in
  Super_context.add_rule sctx ~dir ~mode ~loc action
;;

let gen_rules sctx ~dir_contents ~dir ~for_ =
  let open Memo.O in
  let ocamllex_or_ocamlyacc, modules_for =
    match for_ with
    | Parser_generators.Ocamllex s -> s, Ml_sources.Parser_generators.Ocamllex s.loc
    | Ocamlyacc s -> s, Ocamlyacc s.loc
  in
  Dir_contents.ocaml dir_contents
  >>| Ml_sources.Parser_generators.modules ~for_:modules_for
  >>= function
  | None -> Memo.return ()
  | Some targets ->
    let { Parser_generators.loc; mode; _ } = ocamllex_or_ocamlyacc in
    Module_trie.to_map targets
    |> Module_name.Map.values
    |> Memo.parallel_iter ~f:(add_rule sctx ~dir ~loc ~mode ~for_)
;;

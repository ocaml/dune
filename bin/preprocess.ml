open Stdune
open Import

let doc = "Print the preprocessed output"

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|$(b,dune preprocess FILE) build the given FILE and print the preprocessed output|}
  ; `Blocks Common.help_secs
  ]

let info = Term.info "preprocess" ~doc ~man

let change_ext f path =
  let base, ext = Path.of_string path |> Path.split_extension in
  let suffix = f ext in
  Path.extend_basename base ~suffix |> Path.to_string

let pp_with_ocamlc sctx project pp_ml =
  let open Dune_engine in
  let ocamlc = (Super_context.context sctx).ocamlc in
  let env = Super_context.context_env sctx in
  let argv = ["-stop-after"; "parsing"; "-dsource"; pp_ml; "-dump-into-file"] in
  let open Fiber.O in
  let* _ = Process.run ~env Process.Strict ocamlc argv in
  let dump_file =
    pp_ml |> change_ext (fun ext ->
      let dialect =
        Dialect.DB.find_by_extension (Dune_project.dialects project) ext
      in
      match dialect with
      | None -> User_error.raise [ Pp.textf "unsupported extension: %s" ext ]
      | Some (_, kind) ->
        let open Import.Ml_kind in
        match kind with
        | Intf -> ".cmi.dump"
        | Impl -> ".cmo.dump"
    )
  in
  let dump_file_path = Path.of_string dump_file in
  if not (Path.exists dump_file_path && Path.is_file dump_file_path) then
    User_error.raise [ Pp.textf "cannot find a dump file: %s" dump_file ]
  else
    In_channel.with_open_text dump_file (fun ic -> Io.copy_channels ic stdout);
    Sys.remove dump_file;
    exit 0

let term =
  let+ common = Common.term
  and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE")
  and+ ctx_name =
    Common.context_arg ~doc:{|Select context where to build.|} in
  let config = Common.init common in
  let file = Common.prefix_target common file in
  if String.is_empty file then
    User_error.raise [ Pp.textf "no file is given" ];
  let pp_file = file |> change_ext (fun ext -> ".pp" ^ ext) in
  let sctx, project, path =
    Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      Build_system.run_exn (fun () ->
        let open Memo.O in
        let* setup = setup in
        let* project =
          Dune_engine.Source_tree.root () >>| Dune_engine.Source_tree.Dir.project
        in
        let context = Import.Main.find_context_exn setup ~name:ctx_name in
        let sctx = Import.Main.find_scontext_exn setup ~name:ctx_name in
        let target = Path.build (Path.Build.relative context.build_dir pp_file) in
        Build_system.file_exists target >>= function
        | false ->
          User_error.raise
            [ Pp.textf "%s is not preprocessed" (String.maybe_quoted file) ]
        | true ->
          let+ _digest = Build_system.build_file target in
          (sctx, project, Path.to_string target)))
  in
  Hooks.End_of_build.run ();
  Scheduler.go ~common ~config (fun () ->
    pp_with_ocamlc sctx project path
  )

let command = (term, info)
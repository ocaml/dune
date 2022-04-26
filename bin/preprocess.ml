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

let pp_with_ocamlc sctx project pp_ml =
  let open Dune_engine in
  let ocamlc = (Super_context.context sctx).ocamlc in
  let env = Super_context.context_env sctx in
  let argv =
    [ "-stop-after"
    ; "parsing"
    ; "-dsource"
    ; Path.to_string pp_ml
    ; "-dump-into-file"
    ]
  in
  let open Fiber.O in
  let+ () = Process.run ~env Process.Strict ocamlc argv in
  let dump_file =
    Path.map_extension pp_ml ~f:(fun ext ->
        let dialect =
          Dialect.DB.find_by_extension (Dune_project.dialects project) ext
        in
        match dialect with
        | None -> User_error.raise [ Pp.textf "unsupported extension: %s" ext ]
        | Some (_, kind) -> (
          let open Import.Ml_kind in
          match kind with
          | Intf -> ".cmi.dump"
          | Impl -> ".cmo.dump"))
  in
  if not (Path.exists dump_file && Path.is_file dump_file) then
    User_error.raise
      [ Pp.textf "cannot find a dump file: %s" (Path.to_string dump_file) ]
  else Io.cat dump_file;
  Path.unlink_no_err dump_file;
  ()

let term =
  let+ common = Common.term
  and+ file = Arg.(value & pos 0 string "" & info [] ~docv:"FILE")
  and+ ctx_name = Common.context_arg ~doc:{|Select context where to build.|} in
  let config = Common.init common in
  let file =
    let file = Common.prefix_target common file in
    if String.is_empty file then
      User_error.raise [ Pp.textf "no file is given" ]
    else Path.of_string file
  in
  let pp_file = file |> Path.map_extension ~f:(fun ext -> ".pp" ^ ext) in
  let result =
    Scheduler.go ~common ~config (fun () ->
        let open Fiber.O in
        let* setup = Import.Main.setup () in
        Build_system.run_exn (fun () ->
            let open Memo.O in
            let* setup = setup in
            let* project =
              Dune_engine.Source_tree.root ()
              >>| Dune_engine.Source_tree.Dir.project
            in
            let context = Import.Main.find_context_exn setup ~name:ctx_name in
            let sctx = Import.Main.find_scontext_exn setup ~name:ctx_name in
            let in_build_dir file =
              file |> Path.to_string
              |> Path.Build.relative context.build_dir
              |> Path.build
            in
            let pp_file = in_build_dir pp_file in
            Build_system.file_exists pp_file >>= function
            | true ->
              let+ _digest = Build_system.build_file pp_file in
              Ok (sctx, project, pp_file)
            | false -> (
              let file = in_build_dir file in
              Build_system.file_exists file >>= function
              | true ->
                let+ _digest = Build_system.build_file file in
                Error file
              | false ->
                User_error.raise
                  [ Pp.textf "%s does not exist" (Path.to_string file) ])))
  in
  Hooks.End_of_build.run ();
  match result with
  | Ok (sctx, project, path) ->
    Scheduler.go ~common ~config (fun () -> pp_with_ocamlc sctx project path)
  | Error path -> Io.cat path

let command = (term, info)

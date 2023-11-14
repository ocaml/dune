open Import

let pp_with_ocamlc env ~ocamlc dialects pp_file =
  let open Dune_engine in
  let dump_file =
    Path.map_extension pp_file ~f:(fun ext ->
      let dialect = Dune_rules.Dialect.DB.find_by_extension dialects ext in
      match dialect with
      | None -> User_error.raise [ Pp.textf "unsupported extension: %s" ext ]
      | Some (_, (kind : Ocaml.Ml_kind.t)) ->
        (match kind with
         | Intf -> ".cmi.dump"
         | Impl -> ".cmo.dump"))
  in
  let open Fiber.O in
  let+ () =
    Process.run
      ~display:!Clflags.display
      ~env
      Strict
      ocamlc
      [ "-stop-after"; "parsing"; "-dsource"; Path.to_string pp_file; "-dump-into-file" ]
  in
  match Path.stat dump_file with
  | Ok { st_kind = S_REG; _ } ->
    Io.cat dump_file;
    Path.unlink_no_err dump_file
  | _ ->
    User_error.raise [ Pp.textf "cannot find a dump file: %s" (Path.to_string dump_file) ]
;;

let get_pped_file super_context file =
  let open Memo.O in
  let context = Super_context.context super_context in
  let in_build_dir file =
    file |> Path.to_string |> Path.Build.relative (Context.build_dir context)
  in
  let file_in_build_dir =
    if String.is_empty file
    then User_error.raise [ Pp.textf "No file given." ]
    else Path.of_string file |> in_build_dir |> Path.build
  in
  let pp_file = file_in_build_dir |> Path.map_extension ~f:(fun ext -> ".pp" ^ ext) in
  Build_system.file_exists pp_file
  >>= function
  | true ->
    let* () = Build_system.build_file pp_file in
    let+ project = Source_tree.root () >>| Source_tree.Dir.project in
    Ok (project, pp_file)
  | false ->
    Build_system.file_exists file_in_build_dir
    >>= (function
     | true ->
       let* dir =
         Source_tree.nearest_dir (Path.Source.of_string file)
         >>| Source_tree.Dir.path
         >>| Path.source
       in
       let* dune_file = Dune_rules.Dune_load.Dune_files.in_dir (dir |> in_build_dir) in
       let staged_pps =
         Option.bind dune_file ~f:(fun dune_file ->
           dune_file.stanzas
           |> List.fold_left ~init:None ~f:(fun acc stanza ->
             match stanza with
             | Dune_rules.Dune_file.Library lib ->
               let preprocess =
                 Dune_rules.Preprocess.Per_module.(
                   lib.buildable.preprocess |> single_preprocess)
               in
               (match preprocess with
                | Dune_rules.Preprocess.Pps ({ staged = true; _ } as pps) -> Some pps
                | _ -> acc)
             | _ -> acc))
       in
       (match staged_pps with
        | None ->
          let+ () = Build_system.build_file file_in_build_dir in
          Error file_in_build_dir
        | Some { loc; _ } ->
          User_error.raise ~loc [ Pp.text "staged_pps are not supported." ])
     | false ->
       User_error.raise
         [ Pp.textf "%s does not exist" (Path.to_string_maybe_quoted file_in_build_dir) ])
;;

let term =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:"Build context to use."
  and+ _ = Describe_lang_compat.arg
  and+ file = Arg.(required & pos 0 (some string) None (Arg.info [] ~docv:"FILE")) in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config
  @@ fun () ->
  let open Fiber.O in
  let* setup = Import.Main.setup () in
  let* setup = Memo.run setup in
  let super_context = Import.Main.find_scontext_exn setup ~name:context_name in
  Build_system.run_exn
  @@ fun () ->
  let open Memo.O in
  let* result = get_pped_file super_context file in
  match result with
  | Error file -> Io.cat file |> Memo.return
  | Ok (project, file) ->
    let* ocamlc =
      let+ ocaml = Context.ocaml (Super_context.context super_context) in
      ocaml.ocamlc
    in
    let env = Super_context.context_env super_context in
    let dialects = Dune_project.dialects project in
    pp_with_ocamlc env ~ocamlc dialects file |> Memo.of_non_reproducible_fiber
;;

let command =
  let doc = "Build a given FILE and print the preprocessed output." in
  let info = Cmd.info ~doc "pp" in
  Cmd.v info term
;;

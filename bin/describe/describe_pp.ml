open Import

let print_pped_file sctx file pp_file =
  let open Memo.O in
  let* loc, action =
    let+ dialect, ml_kind =
      let _base, ext =
        let file = Path.of_string file in
        Path.split_extension file
      in
      let+ project = Source_tree.root () >>| Source_tree.Dir.project in
      let dialects = Dune_project.dialects project in
      match Dune_rules.Dialect.DB.find_by_extension dialects ext with
      | None -> User_error.raise [ Pp.textf "unsupported extension: %s" ext ]
      | Some x -> x
    in
    match Dune_rules.Dialect.print_ast dialect ml_kind with
    | Some print_ast -> print_ast
    | None ->
      (* fall back to the OCaml print_ast function, known to exist, if one
         doesn't exist for this dialect. *)
      Dune_rules.Dialect.print_ast Dune_rules.Dialect.ocaml ml_kind |> Option.value_exn
  in
  let dir = pp_file |> Path.parent_exn |> Path.as_in_build_dir_exn in
  let* action, observing_facts =
    let* build =
      let+ expander =
        let bindings =
          Dune_lang.Pform.Map.singleton
            (Var Input_file)
            [ Dune_lang.Value.Path (Path.build (pp_file |> Path.as_in_build_dir_exn)) ]
        in
        Super_context.expander sctx ~dir >>| Dune_rules.Expander.add_bindings ~bindings
      in
      Dune_rules.For_tests.Action_unexpanded.expand_no_targets
        action
        ~chdir:(Dune_rules.Expander.context expander |> Context_name.build_dir)
        ~loc
        ~expander
        ~deps:[]
        ~what:"describe pp"
    in
    Action_builder.evaluate_and_collect_facts build
  in
  Build_system.execute_action ~observing_facts { action; loc; dir; alias = None }
;;

let files_for_source file ~dialects =
  let base, ext = Path.split_extension file in
  let dialect, kind =
    match Dune_rules.Dialect.DB.find_by_extension dialects ext with
    | None -> User_error.raise [ Pp.textf "unsupported extension: %s" ext ]
    | Some x -> x
  in
  let pp_file_base = Path.extend_basename base ~suffix:ext in
  let pp_files =
    let pp_file_result_base =
      Path.map_extension pp_file_base ~f:(fun ext -> ".pp" ^ ext)
    in
    match Dune_rules.Dialect.ml_suffix dialect kind with
    | None ->
      (* No `.ml` suffix for this dialect. this is already an `.ml` file.
         The extension for the pped file is `.pp.ml` *)
      [ pp_file_result_base ]
    | Some suffix ->
      (* If there's an `.ml` suffix for this dialect, append it to the source.
         In this case, we need to try a few targets:

         - For files preprocessed with ppx: `<original-file>.<original-ext>.pp.ml`.
         - For files preprocessed with actions: `<original-file>.pp.<original-ext>.ml`.
      *)
      [ pp_file_base |> Path.extend_basename ~suffix:".pp" |> Path.extend_basename ~suffix
      ; Path.extend_basename pp_file_result_base ~suffix
      ]
  in
  pp_files
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
  let* pp_files_to_check =
    let+ project = Source_tree.root () >>| Source_tree.Dir.project in
    let dialects = Dune_project.dialects project in
    files_for_source file_in_build_dir ~dialects
  in
  let* pp_file =
    Memo.parallel_map pp_files_to_check ~f:(fun file ->
      let+ exists = Build_system.file_exists file in
      Option.some_if exists file)
    >>| List.find_map ~f:Fun.id
  in
  match pp_file with
  | Some pp_file ->
    let+ () = Build_system.build_file pp_file in
    Ok pp_file
  | None ->
    Build_system.file_exists file_in_build_dir
    >>= (function
     | false ->
       User_error.raise
         [ Pp.textf "%s does not exist" (Path.to_string_maybe_quoted file_in_build_dir) ]
     | true ->
       Source_tree.nearest_dir (Path.Source.of_string file)
       >>| Source_tree.Dir.path
       >>| Path.source
       >>| in_build_dir
       >>= Dune_rules.Dune_load.stanzas_in_dir
       >>= (function
              | None -> Memo.return None
              | Some dune_file ->
                Dune_file.find_stanzas dune_file Dune_rules.Library.key
                >>| List.fold_left ~init:None ~f:(fun acc (lib : Dune_rules.Library.t) ->
                  let preprocess =
                    Dune_rules.Preprocess.Per_module.(
                      lib.buildable.preprocess |> single_preprocess)
                  in
                  match preprocess with
                  | Dune_rules.Preprocess.Pps ({ staged = true; _ } as pps) -> Some pps
                  | _ -> acc))
       >>= (function
        | None ->
          let+ () = Build_system.build_file file_in_build_dir in
          Error file_in_build_dir
        | Some { loc; _ } ->
          User_error.raise ~loc [ Pp.text "staged_pps are not supported." ]))
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
  | Ok pp_file -> print_pped_file super_context file pp_file
;;

let command =
  let doc = "Build a given FILE and print the preprocessed output." in
  let info = Cmd.info ~doc "pp" in
  Cmd.v info term
;;

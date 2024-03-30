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

let in_build_dir context file =
  file |> Path.to_string |> Path.Build.relative (Context.build_dir context)
;;

let module_for_file =
  let rec closest_dune_file dir =
    let open Memo.O in
    let* dune_file = Dune_rules.Dune_load.stanzas_in_dir dir in
    match dune_file with
    | None ->
      (match Path.Build.parent dir with
       | None -> Memo.return None
       | Some parent_dir -> closest_dune_file parent_dir)
    | Some dune_file -> Memo.return (Some dune_file)
  in
  let module_for_src =
    let exception Local of Dune_rules.Module.t in
    fun modules ~ml_kind file ->
      try
        Dune_rules.Modules.fold_user_written modules ~init:() ~f:(fun m () ->
          Dune_rules.Module.source m ~ml_kind
          |> Option.iter ~f:(fun src ->
            if Path.equal file (Dune_rules.Module.File.path src) then raise (Local m)));
        None
      with
      | Local m -> Some m
  in
  fun ~sctx ~ml_kind file ->
    let open Memo.O in
    let* dir =
      let+ dir =
        Source_tree.nearest_dir (Path.drop_optional_build_context_src_exn file)
        >>| Source_tree.Dir.path
      in
      in_build_dir (Super_context.context sctx) (Path.source dir)
    in
    let* dune_file =
      (* This module could be inside an `include_subdirs` directory. *)
      closest_dune_file dir
    in
    match dune_file with
    | None -> Memo.return None
    | Some dune_file ->
      Dune_rules.Dune_file.stanzas dune_file
      >>= Memo.List.find_map ~f:(fun stanza ->
        let for_and_preprocess =
          match Stanza.repr stanza with
          | Library.T lib ->
            Some
              ( Dune_rules.Ml_sources.Library (Library.best_name lib)
              , lib.buildable.preprocess )
          | Executables.T exes ->
            Some (Exe { first_exe = snd (List.hd exes.names) }, exes.buildable.preprocess)
          | Melange_stanzas.Emit.T mel ->
            Some (Melange { target = mel.target }, mel.preprocess)
          | _ -> None
        in
        match for_and_preprocess with
        | None -> Memo.return None
        | Some (for_, preprocess) ->
          let+ modules =
            Dune_rules.Dir_contents.get sctx ~dir
            >>= Dune_rules.Dir_contents.ocaml
            >>| Dune_rules.Ml_sources.modules ~for_
          in
          module_for_src modules ~ml_kind file |> Option.map ~f:(fun m -> m, preprocess))
      >>= (function
       | None -> Memo.return None
       | Some (m, preprocess) ->
         let+ pped_map =
           let+ version =
             let+ ocaml = Context.ocaml (Super_context.context sctx) in
             ocaml.version
           and+ preprocess =
             let* scope = Dune_rules.Scope.DB.find_by_dir dir in
             Resolve.Memo.read_memo
               (Dune_rules.Preprocess.Per_module.with_instrumentation
                  preprocess
                  ~instrumentation_backend:
                    (Dune_rules.Lib.DB.instrumentation_backend
                       (Dune_rules.Scope.libs scope)))
           in
           Staged.unstage (Dune_rules.Preprocessing.pped_modules_map preprocess version)
         in
         Some (pped_map m))
;;

let get_pped_file super_context file =
  let open Memo.O in
  let in_build_dir =
    let context = Super_context.context super_context in
    in_build_dir context
  in
  let file_in_build_dir =
    if String.is_empty file
    then User_error.raise [ Pp.textf "No file given." ]
    else Path.of_string file |> in_build_dir |> Path.build
  in
  let* pp_file =
    let* pp_file =
      let* ml_kind =
        let+ project = Source_tree.root () >>| Source_tree.Dir.project in
        let dialects = Dune_project.dialects project in
        let _base, ext = Path.split_extension file_in_build_dir in
        match Dune_rules.Dialect.DB.find_by_extension dialects ext with
        | None -> User_error.raise [ Pp.textf "unsupported extension: %s" ext ]
        | Some (_, ml_kind) -> ml_kind
      in
      let+ m = module_for_file ~sctx:super_context ~ml_kind file_in_build_dir in
      m
      |> Option.bind ~f:(Dune_rules.Module.source ~ml_kind)
      |> Option.map ~f:Dune_rules.Module.File.path
    in
    match pp_file with
    | None -> Memo.return None
    | Some file ->
      let+ exists = Build_system.file_exists file in
      Option.some_if exists file
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

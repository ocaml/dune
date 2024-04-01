open Import

let dialect_and_ml_kind file =
  let open Memo.O in
  let _base, ext =
    let file = Path.of_string file in
    Path.split_extension file
  in
  let+ project = Source_tree.root () >>| Source_tree.Dir.project in
  let dialects = Dune_project.dialects project in
  match Dune_rules.Dialect.DB.find_by_extension dialects ext with
  | None -> User_error.raise [ Pp.textf "unsupported extension: %s" ext ]
  | Some x -> x
;;

let execute_pp_action ~sctx file pp_file dump_file =
  let open Memo.O in
  let* expander =
    let bindings =
      Dune_lang.Pform.Map.singleton
        (Var Input_file)
        [ Dune_lang.Value.Path (Path.build (pp_file |> Path.as_in_build_dir_exn)) ]
    in
    let dir = pp_file |> Path.parent_exn |> Path.as_in_build_dir_exn in
    Super_context.expander sctx ~dir >>| Dune_rules.Expander.add_bindings ~bindings
  in
  let context = Dune_rules.Expander.context expander in
  let build_dir = Context_name.build_dir context in
  let* input =
    let* action, _observing_facts =
      let* loc, action =
        let+ dialect, ml_kind = dialect_and_ml_kind file in
        match Dune_rules.Dialect.print_ast dialect ml_kind with
        | Some print_ast -> print_ast
        | None ->
          (* fall back to the OCaml print_ast function, known to exist, if one
             doesn't exist for this dialect. *)
          Dune_rules.Dialect.print_ast Dune_rules.Dialect.ocaml ml_kind
          |> Option.value_exn
      in
      let build =
        let open Action_builder.O in
        let+ build =
          Dune_rules.For_tests.Action_unexpanded.expand_no_targets
            action
            ~chdir:build_dir
            ~loc
            ~expander
            ~deps:[]
            ~what:"describe pp"
        in
        Action.with_outputs_to dump_file build.action
      in
      Action_builder.evaluate_and_collect_facts build
    in
    let+ env = Dune_rules.Super_context.context_env sctx
    and+ execution_parameters = Dune_engine.Execution_parameters.default in
    let targets =
      let unvalidated = Targets.File.create dump_file in
      match Targets.validate unvalidated with
      | Valid targets -> targets
      | No_targets
      | Inconsistent_parent_dir
      | File_and_directory_target_with_the_same_name _ -> assert false
    in
    { Dune_engine.Action_exec.targets = Some targets
    ; root = Path.build build_dir
    ; context = Some (Dune_engine.Build_context.create ~name:context)
    ; env
    ; rule_loc = Loc.none
    ; execution_parameters
    ; action
    }
  in
  let ok =
    let open Fiber.O in
    let build_deps deps = Build_system.build_deps deps |> Memo.run in
    let* result = Dune_engine.Action_exec.exec input ~build_deps in
    Dune_engine.Action_exec.Exec_result.ok_exn result >>| ignore
  in
  Memo.of_non_reproducible_fiber ok
;;

let print_pped_file =
  let dump_file pp_file ~ml_kind =
    Path.set_extension
      pp_file
      ~ext:
        (match (ml_kind : Ocaml.Ml_kind.t) with
         | Intf -> ".cmi.dump"
         | Impl -> ".cmo.dump")
    |> Path.as_in_build_dir_exn
  in
  fun ~sctx file pp_file ~ml_kind ->
    let open Memo.O in
    let dump_file = dump_file pp_file ~ml_kind in
    let+ () = execute_pp_action ~sctx file pp_file dump_file in
    let dump_file = Path.build dump_file in
    match Path.stat dump_file with
    | Ok { st_kind = S_REG; _ } ->
      Io.cat dump_file;
      Path.unlink_no_err dump_file
    | _ ->
      User_error.raise
        [ Pp.textf "cannot find a dump file: %s" (Path.to_string dump_file) ]
;;

let find_module ~sctx file =
  let open Memo.O in
  let src = Path.drop_optional_build_context_src_exn (Path.build file) in
  Dune_rules.Top_module.find_module sctx src
  >>| function
  | None -> None
  | Some (m, _, _, origin) ->
    (match
       Dune_rules.Ml_sources.Origin.preprocess origin
       |> Dune_rules.Preprocess.Per_module.find (Dune_rules.Module.name m)
     with
     | Pps { staged = true; loc; _ } -> Some (`Staged_pps loc)
     | _ -> Some (`Module m))
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
    else Path.of_string file |> in_build_dir
  in
  let* ml_kind =
    let+ _, ml_kind = dialect_and_ml_kind file in
    ml_kind
  in
  let file_not_found () =
    User_error.raise
      [ Pp.textf "%s does not exist" (Path.Build.to_string_maybe_quoted file_in_build_dir)
      ]
  in
  find_module ~sctx:super_context file_in_build_dir
  >>= function
  | None -> file_not_found ()
  | Some (`Module m) ->
    (match
       Dune_rules.Module.source m ~ml_kind |> Option.map ~f:Dune_rules.Module.File.path
     with
     | None -> file_not_found ()
     | Some pp_file ->
       let+ () = Build_system.build_file pp_file in
       Ok (pp_file, ml_kind))
  | Some (`Staged_pps loc) ->
    User_error.raise ~loc [ Pp.text "staged_pps are not supported." ]
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
  let sctx = Import.Main.find_scontext_exn setup ~name:context_name in
  Build_system.run_exn
  @@ fun () ->
  let open Memo.O in
  let* result = get_pped_file sctx file in
  match result with
  | Error file -> Io.cat file |> Memo.return
  | Ok (pp_file, ml_kind) -> print_pped_file ~sctx file pp_file ~ml_kind
;;

let command =
  let doc = "Build a given FILE and print the preprocessed output." in
  let info = Cmd.info ~doc "pp" in
  Cmd.v info term
;;

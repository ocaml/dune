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

let print_pped_file sctx file pp_file =
  let open Memo.O in
  let* loc, action =
    let+ dialect, ml_kind = dialect_and_ml_kind file in
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
       Ok pp_file)
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

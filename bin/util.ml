open Import

let find_in_path_exn prog =
  match Bin.which ~path:(Env_path.path Env.initial) prog with
  | Some path -> path
  | None -> User_error.raise [ Pp.textf "unable to find %s in PATH" prog ]
;;

let has_directory_component prog =
  String.exists prog ~f:(function
    | '/' | '\\' -> true
    | _ -> false)
;;

let resolve_prog prog =
  if has_directory_component prog then prog else Path.to_string (find_in_path_exn prog)
;;

let resolve_program_path prog =
  if Filename.is_relative prog && not (has_directory_component prog)
  then find_in_path_exn prog
  else Path.of_filename_relative_to_initial_cwd prog
;;

type checked =
  | In_build_dir of (Context.t * Path.Source.t)
  | In_private_context of Path.Build.t
  | In_install_dir of (Context.t * Path.Source.t)
  | In_source_dir of Path.Source.t
  | External of Path.External.t

let check_path contexts =
  let contexts =
    Dune_engine.Context_name.Map.of_list_map_exn contexts ~f:(fun c -> Context.name c, c)
  in
  fun path ->
    let internal_path () =
      User_error.raise
        [ Pp.textf "This path is internal to dune: %s" (Path.to_string_maybe_quoted path)
        ]
    in
    let context_exn ctx =
      match Dune_engine.Context_name.Map.find contexts ctx with
      | Some context -> context
      | None ->
        User_error.raise
          [ Pp.textf
              "%s refers to unknown build context: %s"
              (Path.to_string_maybe_quoted path)
              (Dune_engine.Context_name.to_string ctx)
          ]
          ~hints:
            (User_message.did_you_mean
               (Dune_engine.Context_name.to_string ctx)
               ~candidates:
                 (Dune_engine.Context_name.Map.keys contexts
                  |> List.map ~f:Dune_engine.Context_name.to_string))
    in
    match path with
    | External e -> External e
    | In_source_tree s -> In_source_dir s
    | In_build_dir path ->
      (match Dune_engine.Dpath.analyse_target path with
       | Other _ -> internal_path ()
       | Alias (_, _) -> internal_path ()
       | Anonymous_action _ -> internal_path ()
       | Regular (name, src) ->
         (match Install.Context.analyze_path name src with
          | Invalid -> internal_path ()
          | Install (ctx, path) -> In_install_dir (context_exn ctx, path)
          | Normal (ctx, path) ->
            if Context_name.equal ctx Dune_rules.Private_context.t.name
            then
              In_private_context
                (Path.Build.append_source Dune_rules.Private_context.t.build_dir path)
            else In_build_dir (context_exn ctx, path)))
;;

let restore_cwd_and_execve (root : Workspace_root.t) prog args env =
  let prog = if Filename.is_relative prog then Filename.concat root.dir prog else prog in
  Proc.restore_cwd_and_execve prog args ~env
;;

let setup () =
  let scheduler = Scheduler.t () in
  Console.Status_line.set
    (Live
       (fun () ->
         match !Build_system.state with
         | Initializing
         | Restarting_current_build
         | Build_succeeded__now_waiting_for_changes
         | Build_failed__now_waiting_for_changes -> Pp.nop
         | Building
             { Build_system.Progress.number_of_rules_validated = done_
             ; number_of_rules_discovered = total
             ; number_of_rules_failed = failed
             } ->
           Pp.verbatim
             (sprintf
                "Done: %u%% (%u/%u, %u left%s) (jobs: %u)"
                (if total = 0 then 0 else done_ * 100 / total)
                done_
                total
                (total - done_)
                (if failed = 0 then "" else sprintf ", %u failed" failed)
                (Scheduler.running_jobs_count scheduler))));
  Dune_rules.Main.get ()
;;

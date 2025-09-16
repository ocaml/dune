open Import

let vendor_dir = lazy (Path.of_string "vendor")

module Console = struct
  let verbose = ref false
  let print_always msg = Dune_console.print [ Pp.verbatim msg ]
  let printf_always fmt = Printf.ksprintf print_always fmt
  let print msg = if !verbose then print_always msg
  let printf fmt = Printf.ksprintf print fmt

  let print_status msg ~f =
    if !verbose
    then (
      print msg;
      f ())
    else Dune_console.Status_line.with_overlay (Constant (Pp.verbatim msg)) ~f
  ;;

  let print_status fmt ~f = Printf.ksprintf (print_status ~f) fmt
end

let run_cmd ?dir mode prog args =
  Process.run_capture
    ?dir
    ~display:!Dune_engine.Clflags.display
    ?stderr_to:(Option.some_if (not !Console.verbose) (Process.Io.null Out))
    mode
    prog
    args
;;

module Git = struct
  let git =
    lazy
      (match Bin.which "git" ~path:(Env_path.path Env.initial) with
       | Some git -> git
       | None -> User_error.raise [ Pp.textf "Git not found in PATH." ])
  ;;

  let clone_shallow ?(args = []) ~target_dir url =
    run_cmd
      Strict
      (Lazy.force git)
      (List.concat
         [ [ "clone"; url; Path.to_string_maybe_quoted target_dir ]
         ; [ "--depth"; "1" ]
         ; args
         ])
    >>| ignore
  ;;

  let clone_at_commit ~url ~repo_dir ~commit =
    let* () = clone_shallow url ~target_dir:repo_dir in
    let* () =
      run_cmd
        ~dir:repo_dir
        Strict
        (Lazy.force git)
        [ "fetch"; "origin"; commit; "--depth"; "1" ]
      >>| ignore
    in
    run_cmd ~dir:repo_dir Strict (Lazy.force git) [ "checkout"; commit ] >>| ignore
  ;;

  let ls_remote ~url =
    run_cmd Strict (Lazy.force git) [ "ls-remote"; url ] >>| String.split_lines
  ;;

  let ls_remote_tags ~url =
    run_cmd Strict (Lazy.force git) [ "ls-remote"; "--tags"; url ] >>| String.split_lines
  ;;

  let checkout_file =
    let global_mutex = Fiber.Mutex.create () in
    fun ~file_path ->
      Fiber.Mutex.with_lock global_mutex ~f:(fun () ->
        run_cmd Strict (Lazy.force git) [ "checkout"; "--"; Path.to_string file_path ]
        >>| ignore)
  ;;

  let diff_no_index ~path1 ~path2 =
    run_cmd
      Return
      (Lazy.force git)
      [ "diff"; "--no-index"; Path.to_string path1; Path.to_string path2 ]
    >>| fst
  ;;
end

let with_temp_dir f =
  Temp.with_temp_dir
    ~parent_dir:(Path.of_string (Filename.get_temp_dir_name ()))
    ~prefix:"dune_vendor"
    ~suffix:""
    ~f:(function
      | Ok temp_dir -> f temp_dir
      | Error exn -> raise exn)
;;

let expand_glob pattern base_dir =
  let base_path = Path.of_string base_dir in
  let glob_re =
    Re.Glob.glob ~anchored:true ~expand_braces:true ~double_asterisk:false pattern
    |> Re.compile
  in
  match Path.readdir_unsorted base_path with
  | Ok entries -> List.filter entries ~f:(Re.execp glob_re)
  | Error unix_err -> Unix_error.Detailed.raise unix_err
;;

let execute_copy_rule ~name (rule : (Path.t, Path.t) Packages.Copy_rule.t) =
  match rule with
  | Copy { src; dst_dir } ->
    let+ () = Fiber.return () in
    if Path.exists src
    then (
      Path.mkdir_p dst_dir;
      Io.copy_file ~src ~dst:(Path.relative dst_dir (Path.basename src)) ())
    else User_error.raise [ Pp.textf "Source file not found: %s" (Path.to_string src) ]
  | Copy_rename { src; dst } ->
    let+ () = Fiber.return () in
    if Path.exists src
    then (
      Path.mkdir_p (Path.parent_exn dst);
      Io.copy_file ~src ~dst ())
    else User_error.raise [ Pp.textf "Source file not found: %s" (Path.to_string src) ]
  | Copy_glob { src_dir; pattern; dst_dir } ->
    let files = expand_glob pattern (Path.to_string src_dir) in
    Path.mkdir_p dst_dir;
    Fiber.parallel_iter files ~f:(fun file ->
      let+ () = Fiber.return () in
      Io.copy_file ~src:(Path.relative src_dir file) ~dst:(Path.relative dst_dir file) ())
  | Remove paths ->
    Fiber.parallel_iter paths ~f:(fun path ->
      if Path.exists path
      then (
        Console.printf "Removing: %s" (Path.to_string path);
        let+ () = Fiber.return () in
        Path.rm_rf ~allow_external:true path)
      else
        User_error.raise
          [ Pp.textf "File %s does not exist and cannot be removed" (Path.to_string path)
          ; Pp.textf
              "Please correct or remove the `Remove` constructor from packages.ml for \
               package %S."
              name
          ])
;;

let dune =
  lazy
    (match Bin.which "dune" ~path:(Env_path.path Env.initial) with
     | Some dune -> dune
     | None -> User_error.raise [ Pp.textf "Dune not found in PATH." ])
;;

let fetch_git ~url ~revision ~build_cmd repo_dir =
  let* () =
    Console.print_status "Cloning %s..." url ~f:(fun () ->
      match (revision : Packages.revision) with
      | Tag tag -> Git.clone_shallow ~args:[ "--branch"; tag ] ~target_dir:repo_dir url
      | Commit commit -> Git.clone_at_commit ~url ~repo_dir ~commit)
  in
  let+ () =
    match build_cmd with
    | Some (`Dune args) -> run_cmd ~dir:repo_dir Strict (Lazy.force dune) args >>| ignore
    | None -> Fiber.return ()
  in
  repo_dir
;;

let validate_package_names names =
  List.concat_map names ~f:(fun name ->
    let package_name = Path.Local.of_string name |> Path.Local.basename in
    let matching_packages =
      List.filter Packages.all_packages ~f:(fun { Packages.name; _ } ->
        String.equal name package_name)
    in
    match matching_packages with
    | [] ->
      let candidates =
        List.map Packages.all_packages ~f:(fun { Packages.name; _ } -> name)
      in
      User_error.raise
        [ Pp.textf "Package '%s' not found." package_name
        ; Pp.textf "Available packages: %s" (String.enumerate_and candidates)
        ]
        ~hints:(User_message.did_you_mean package_name ~candidates)
    | pkgs -> pkgs)
;;

type common_options =
  { package_names : string list
  ; all_except : bool
  ; verbose : bool
  ; dry_run : bool
  ; concurrency : Dune_config_file.Dune_config.Concurrency.t
  }

let get_packages_to_process { package_names; all_except; _ } =
  if all_except
  then
    List.filter Packages.all_packages ~f:(fun { Packages.name; _ } ->
      not (List.mem package_names name ~equal:String.equal))
  else (
    let validated_names = validate_package_names package_names in
    if List.is_empty validated_names then Packages.all_packages else validated_names)
;;

let run_with_scheduler { verbose; concurrency; _ } f =
  Console.verbose := verbose;
  Console.printf "Working directory: %s" (Sys.getcwd ());
  if verbose then Log.init Stderr else Log.init No_log_file;
  let config =
    Dune_config_file.Dune_config.for_scheduler
      { Dune_config_file.Dune_config.default with
        display =
          (if verbose
           then Dune_config_file.Dune_config.Display.verbose
           else Dune_config_file.Dune_config.Display.quiet)
      ; concurrency
      }
      ~watch_exclusions:[]
      None
      ~print_ctrl_c_warning:true
  in
  try Scheduler.Run.go config ~on_event:(fun _ _ -> ()) ~file_watcher:No_watcher f with
  | Scheduler.Run.Shutdown.E Requested -> ()
  | Scheduler.Run.Shutdown.E (Signal _) -> exit 130
  | exn ->
    let exn = Exn_with_backtrace.capture exn in
    Dune_util.Report_error.report exn;
    exit 1
;;

let cmd_foreach_pkg
      ?(supports_dry_run = true)
      action_desc
      action_fn
      ({ dry_run; _ } as options)
  =
  run_with_scheduler options
  @@ fun () ->
  if dry_run && not supports_dry_run
  then User_error.raise [ Pp.textf "Dry-run mode is not supported for this command." ];
  let packages = get_packages_to_process options in
  let module String_map_parallel = Fiber.Make_parallel_map (String.Map) in
  List.fold_left packages ~init:String.Map.empty ~f:(fun acc spec ->
    String.Map.add_multi acc spec.Packages.name spec)
  |> String_map_parallel.parallel_map ~f:(fun package_name package_specs ->
    Console.print_status "%s %s" action_desc package_name ~f:(fun () ->
      action_fn ~dry_run package_name package_specs))
  >>| ignore
;;

let package_term =
  let open Cmdliner in
  let+ package_names =
    Arg.(
      value
      & pos_all string []
      & info [] ~docv:"PACKAGE" ~doc:"Package name(s) (default: all packages)")
  and+ all_except =
    Arg.(
      value & flag & info [ "all-except" ] ~doc:"Treat package arguments as exclusions")
  and+ verbose = Arg.(value & flag & info [ "verbose"; "v" ] ~doc:"Verbose output")
  and+ dry_run =
    Arg.(
      value
      & flag
      & info [ "dry-run" ] ~doc:"Check integrity without writing (patch only)")
  and+ concurrency =
    let module Concurrency = Dune_config_file.Dune_config.Concurrency in
    let arg =
      Arg.conv
        ( (fun s -> Result.map_error (Concurrency.of_string s) ~f:(fun s -> `Msg s))
        , fun pp x -> Format.pp_print_string pp (Concurrency.to_string x) )
    in
    Arg.(
      value & opt (some arg) None & info [ "j"; "jobs" ] ~doc:"Number of concurrent jobs")
  in
  let concurrency = Option.value concurrency ~default:Auto in
  { package_names; all_except; verbose; dry_run; concurrency }
;;

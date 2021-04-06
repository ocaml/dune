open Stdune
open Dune_engine
module Term = Cmdliner.Term
module Manpage = Cmdliner.Manpage
module Super_context = Dune_rules.Super_context
module Context = Dune_rules.Context
module Config = Dune_util.Config
module Local_install_path = Dune_engine.Local_install_path
module Lib_name = Dune_engine.Lib_name
module Build_system = Dune_engine.Build_system
module Findlib = Dune_rules.Findlib
module Package = Dune_engine.Package
module Dune_package = Dune_rules.Dune_package
module Hooks = Dune_engine.Hooks
module Action_builder = Dune_engine.Action_builder
module Action = Dune_engine.Action
module Dep = Dune_engine.Dep
module Action_to_sh = Dune_engine.Action_to_sh
module Dpath = Dune_engine.Dpath
module Install = Dune_engine.Install
module Section = Dune_engine.Section
module Watermarks = Dune_rules.Watermarks
module Promotion = Dune_engine.Promotion
module Colors = Dune_rules.Colors
module Dune_project = Dune_engine.Dune_project
module Workspace = Dune_rules.Workspace
module Cached_digest = Dune_engine.Cached_digest
module Profile = Dune_rules.Profile
module Log = Dune_util.Log
module Dune_rpc = Dune_rpc_private
include Common.Let_syntax

let in_group (t, info) = (Term.Group.Term t, info)

let make_cache (config : Dune_config.t) =
  let make_cache () =
    let command_handler (Cache.Dedup file) =
      match Build_system.get_cache () with
      | None -> Code_error.raise "deduplication message and no caching" []
      | Some caching ->
        Scheduler.send_sync_task (fun () ->
            let (module Caching : Cache.Caching) = caching.cache in
            match Cached_digest.peek_file (Path.build file.path) with
            | None -> ()
            | Some d when not (Digest.equal d file.digest) -> ()
            | _ -> Caching.Cache.deduplicate Caching.cache file)
    in
    match config.cache_transport with
    | Dune_config.Caching.Transport.Direct ->
      Log.info [ Pp.text "enable binary cache in direct access mode" ];
      let cache =
        Result.ok_exn
          (Result.map_error
             ~f:(fun s -> User_error.E (User_error.make [ Pp.text s ]))
             (Cache.Local.make ?duplication_mode:config.cache_duplication
                ~command_handler ()))
      in
      Cache.make_caching (module Cache.Local) cache
    | Daemon ->
      Log.info [ Pp.text "enable binary cache in daemon mode" ];
      let cache =
        Result.ok_exn
          (Cache.Client.make ?duplication_mode:config.cache_duplication
             ~command_handler ())
      in
      Cache.make_caching (module Cache.Client) cache
  in
  Fiber.return
    (match config.cache_mode with
    | Dune_config.Caching.Mode.Enabled ->
      Some
        { Build_system.cache = make_cache ()
        ; check_probability = config.cache_check_probability
        }
    | Dune_config.Caching.Mode.Disabled ->
      Log.info [ Pp.text "disable binary cache" ];
      None)

module Main = struct
  include Dune_rules.Main

  let scan_workspace = scan_workspace

  let setup ?build_mutex common config =
    let open Fiber.O in
    let* caching = make_cache config in
    let* workspace = Memo.Build.run (scan_workspace ()) in
    let* only_packages =
      match Common.only_packages common with
      | None -> Fiber.return None
      | Some { Common.Only_packages.names; command_line_option } ->
        Package.Name.Set.iter names ~f:(fun pkg_name ->
            if not (Package.Name.Map.mem workspace.conf.packages pkg_name) then
              let pkg_name = Package.Name.to_string pkg_name in
              User_error.raise
                [ Pp.textf "I don't know about package %s (passed through %s)"
                    pkg_name command_line_option
                ]
                ~hints:
                  (User_message.did_you_mean pkg_name
                     ~candidates:
                       (Package.Name.Map.keys workspace.conf.packages
                       |> List.map ~f:Package.Name.to_string)));
        Fiber.sequential_map (Package.Name.Map.values workspace.conf.packages)
          ~f:(fun pkg ->
            let+ vendored =
              let dir = Package.dir pkg in
              Memo.Build.run (Dune_engine.Source_tree.is_vendored dir)
            in
            let name = Package.name pkg in
            let included = Package.Name.Set.mem names name in
            if vendored && included then
              User_error.raise
                [ Pp.textf
                    "Package %s is vendored and so will never be masked. It \
                     makes no sense to pass it to -p, --only-packages or \
                     --for-release-of-packages."
                    (Package.Name.to_string name)
                ];
            Option.some_if (vendored || included) (name, pkg))
        >>| List.filter_map ~f:Fun.id >>| Package.Name.Map.of_list_exn
        >>| Option.some
    in
    let stats = Common.stats common in
    init_build_system workspace ?stats
      ~sandboxing_preference:config.sandboxing_preference ?caching ?build_mutex
      ?only_packages
end

module Scheduler = struct
  include Dune_engine.Scheduler

  let maybe_clear_screen (dune_config : Dune_config.t) =
    match dune_config.terminal_persistence with
    | Clear_on_rebuild -> Console.reset ()
    | Preserve ->
      Console.print_user_message
        (User_message.make
           [ Pp.nop
           ; Pp.tag User_message.Style.Success
               (Pp.verbatim "********** NEW BUILD **********")
           ; Pp.nop
           ])

  let on_event dune_config _config = function
    | Scheduler.Run.Event.Tick -> Console.Status_line.refresh ()
    | Scheduler.Run.Event.Source_files_changed -> maybe_clear_screen dune_config
    | Build_interrupted ->
      let status_line =
        Some
          (Pp.seq
             (* XXX Why do we print "Had errors"? The user simply edited a file *)
             (Pp.tag User_message.Style.Error (Pp.verbatim "Had errors"))
             (Pp.verbatim ", killing current build..."))
      in
      Console.Status_line.set (Fun.const status_line)
    | Build_finish res ->
      let message =
        match res with
        | Success -> Pp.tag User_message.Style.Success (Pp.verbatim "Success")
        | Failure -> Pp.tag User_message.Style.Error (Pp.verbatim "Had errors")
      in
      Console.Status_line.set
        (Fun.const
           (Some
              (Pp.seq message
                 (Pp.verbatim ", waiting for filesystem changes..."))))

  let go ~(common : Common.t) ~config:dune_config f =
    let stats = Common.stats common in
    let config = Dune_config.for_scheduler dune_config None stats in
    Scheduler.Run.go config ~on_event:(on_event dune_config) f

  let poll ~(common : Common.t) ~config:dune_config ~every ~finally =
    let stats = Common.stats common in
    let rpc_where = Some (Dune_rpc_private.Where.default ()) in
    let config = Dune_config.for_scheduler dune_config rpc_where stats in
    let file_watcher = Common.file_watcher common in
    let run =
      let run () =
        Scheduler.Run.poll (fun () ->
            Fiber.finalize every ~finally:(fun () -> Fiber.return (finally ())))
      in
      match Common.rpc common with
      | None -> run
      | Some rpc ->
        fun () ->
          Fiber.fork_and_join_unit
            (fun () ->
              let rpc_config = Dune_rpc_impl.Server.config rpc in
              let scheduler = Scheduler.csexp_scheduler () in
              let rpc =
                Dune_rpc_impl.Run.of_config rpc_config scheduler config.stats
              in
              Dune_rpc_impl.Run.run rpc)
            run
    in
    Scheduler.Run.go config ~file_watcher ~on_event:(on_event dune_config) run
end

let restore_cwd_and_execve (common : Common.t) prog argv env =
  let prog =
    if Filename.is_relative prog then
      let root = Common.root common in
      Filename.concat root.dir prog
    else
      prog
  in
  Proc.restore_cwd_and_execve prog argv ~env

(* Adapted from
   https://github.com/ocaml/opam/blob/fbbe93c3f67034da62d28c8666ec6b05e0a9b17c/src/client/opamArg.ml#L759 *)
let command_alias cmd name =
  let term, info = cmd in
  let orig = Term.name info in
  let doc = Printf.sprintf "An alias for $(b,%s)." orig in
  let man =
    [ `S "DESCRIPTION"
    ; `P
        (Printf.sprintf "$(mname)$(b, %s) is an alias for $(mname)$(b, %s)."
           name orig)
    ; `P (Printf.sprintf "See $(mname)$(b, %s --help) for details." orig)
    ; `Blocks Common.help_secs
    ]
  in
  (term, Term.info name ~docs:"COMMAND ALIASES" ~doc ~man)

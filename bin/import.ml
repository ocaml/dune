open Stdune
open Dune
module Term = Cmdliner.Term
module Manpage = Cmdliner.Manpage
module Super_context = Dune.Super_context
module Context = Dune.Context
module Config = Dune.Config
module Lib_name = Dune.Lib_name
module Lib_deps_info = Dune.Lib_deps_info
module Build_system = Dune.Build_system
module Findlib = Dune.Findlib
module Package = Dune.Package
module Dune_package = Dune.Dune_package
module Hooks = Dune.Hooks
module Build = Dune.Build
module Action = Dune.Action
module Dep = Dune.Dep
module Action_to_sh = Dune.Action_to_sh
module Dpath = Dune.Dpath
module Install = Dune.Install
module Watermarks = Dune.Watermarks
module Promotion = Dune.Promotion
module Colors = Dune.Colors
module Report_error = Dune.Report_error
module Dune_project = Dune.Dune_project
module Workspace = Dune.Workspace
module Cached_digest = Dune.Cached_digest
module Profile = Dune.Profile
include Common.Let_syntax

let make_cache (config : Config.t) =
  let make_cache () =
    let handle (Dune_cache.Dedup file) =
      match Build_system.get_cache () with
      | None -> Code_error.raise "deduplication message and no caching" []
      | Some caching -> Scheduler.send_dedup caching.cache file
    in
    match config.cache_transport with
    | Config.Caching.Transport.Direct ->
      Log.info "enable binary cache in direct access mode";
      let cache =
        Result.ok_exn
          (Result.map_error
             ~f:(fun s -> User_error.E (User_error.make [ Pp.text s ]))
             (Dune_cache.Cache.make ?duplication_mode:config.cache_duplication
                handle))
      in
      Dune_cache.make_caching (module Dune_cache.Cache) cache
    | Daemon ->
      Log.info "enable binary cache in daemon mode";
      let cache =
        Result.ok_exn
          (Dune_cache_daemon.Client.make
             ?duplication_mode:config.cache_duplication handle)
      in
      Dune_cache.make_caching (module Dune_cache_daemon.Client) cache
  in
  Fiber.return
    ( match config.cache_mode with
    | Config.Caching.Mode.Enabled ->
      Some
        { Build_system.cache = make_cache ()
        ; check_probability = config.cache_check_probability
        }
    | Config.Caching.Mode.Disabled ->
      Log.info "disable binary cache";
      None )

module Main = struct
  include Dune.Main

  let scan_workspace (common : Common.t) =
    let workspace_file =
      Common.workspace_file common |> Option.map ~f:Arg.Path.path
    in
    let x = Common.x common in
    let profile = Common.profile common in
    let capture_outputs = Common.capture_outputs common in
    let ancestor_vcs = (Common.root common).ancestor_vcs in
    scan_workspace ?workspace_file ?x ?profile ~capture_outputs ~ancestor_vcs ()

  let setup ?external_lib_deps_mode common =
    let open Fiber.O in
    let* caching = make_cache (Common.config common) in
    let* workspace = scan_workspace common in
    let only_packages =
      Option.map (Common.only_packages common)
        ~f:(fun { Common.Only_packages.names; command_line_option } ->
          Package.Name.Set.iter names ~f:(fun pkg_name ->
              if not (Package.Name.Map.mem workspace.conf.packages pkg_name)
              then
                let pkg_name = Package.Name.to_string pkg_name in
                User_error.raise
                  [ Pp.textf "I don't know about package %s (passed through %s)"
                      pkg_name command_line_option
                  ]
                  ~hints:
                    (User_message.did_you_mean pkg_name
                       ~candidates:
                         ( Package.Name.Map.keys workspace.conf.packages
                         |> List.map ~f:Package.Name.to_string )));
          Package.Name.Map.filter workspace.conf.packages ~f:(fun pkg ->
              let vendored =
                Dune.File_tree.find_dir pkg.path
                |> Option.value_exn |> Dune.File_tree.Dir.vendored
              in
              let included = Package.Name.Set.mem names pkg.name in
              if vendored && included then
                User_error.raise
                  [ Pp.textf
                      "Package %s is vendored and so will never be masked. It \
                       makes no sense to pass it to -p, --only-packages or \
                       --for-release-of-packages."
                      (Package.Name.to_string pkg.name)
                  ];
              vendored || included))
    in
    init_build_system workspace
      ~sandboxing_preference:(Common.config common).sandboxing_preference
      ?caching ?external_lib_deps_mode ?only_packages
end

module Scheduler = struct
  include Dune.Scheduler
  open Fiber.O

  let go ~(common : Common.t) f =
    let config = Common.config common in
    let f () = Main.set_concurrency config >>= f in
    Scheduler.go ~config f

  let poll ~(common : Common.t) ~once ~finally () =
    let config = Common.config common in
    let once () =
      let* () = Main.set_concurrency config in
      once ()
    in
    Scheduler.poll ~config ~once ~finally ()
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

let do_build targets = Build_system.do_build ~request:(Target.request targets)

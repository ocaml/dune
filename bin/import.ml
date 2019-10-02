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

module Cache_mode = struct
  type t =
    | Daemon
    | Direct

  let of_string = function
    | "daemon" -> Some Daemon
    | "direct" -> Some Direct
    | _ -> None

  let get () =
    let var = "DUNE_CACHE_MODE" in
    match Env.get Env.initial var with
    | None -> Daemon
    | Some v -> (
      match of_string v with
      | Some v -> v
      | None ->
        User_error.raise [ Pp.textf "Unrecognized value for %s: %s" var v ] )
end

let make_cache () =
  let var = "DUNE_CACHE"
  and handle = function
    | Dune_memory.Dedup file -> Scheduler.send_dedup file
  in
  match Env.get Env.initial var with
  | Some v ->
    let cache =
      match Cache_mode.get () with
      | Daemon ->
        let cache =
          Result.ok_exn
            (Result.map_error
               ~f:(fun s -> User_error.E (User_error.make [ Pp.text s ]))
               (Dune_memory.Memory.make handle))
        in
        Dune_memory.make_caching (module Dune_memory.Memory) cache
      | Direct ->
        let cache = Result.ok_exn (Dune_manager.Client.make handle) in
        Dune_memory.make_caching (module Dune_manager.Client) cache
    in
    Fiber.return
      ( match v with
      | v when String.lowercase v = "check" -> Build_system.Check cache
      | "1" -> Build_system.Enabled cache
      | _ -> User_error.raise [ Pp.textf "Unrecognized value for %s: %s" var v ]
      )
  | _ -> Fiber.return Build_system.Disabled

module Main = struct
  include Dune.Driver

  let scan_workspace (common : Common.t) =
    let workspace_file =
      Common.workspace_file common |> Option.map ~f:Arg.Path.path
    in
    let x = Common.x common in
    let profile = Common.profile common in
    let capture_outputs = Common.capture_outputs common in
    let ancestor_vcs = (Common.root common).ancestor_vcs in
    scan_workspace ?workspace_file ?x ?profile ~capture_outputs ~ancestor_vcs
      ()

  let setup ?external_lib_deps_mode common =
    let open Fiber.O in
    let only_packages = Common.only_packages common in
    let* caching = make_cache () in
    let* workspace = scan_workspace common in
    init_build_system workspace
      ~sandboxing_preference:(Common.config common).sandboxing_preference
      ~caching ?external_lib_deps_mode ?only_packages
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

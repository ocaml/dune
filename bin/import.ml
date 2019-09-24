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

let make_memory () =
  let handle = function
    | Dune_manager.Client.Dedup (target, source, digest) ->
      Scheduler.send_dedup target source digest
  in
  match Sys.getenv_opt "DUNE_CACHE" with
  | Some _ ->
    Fiber.return (Some (Result.ok_exn (Dune_manager.Client.make handle)))
  | _ -> Fiber.return None

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
    scan_workspace ?workspace_file ?x ?profile ~capture_outputs ~ancestor_vcs
      ()

  let setup ?external_lib_deps_mode common =
    let open Fiber.O in
    let only_packages = Common.only_packages common in
    let* memory = make_memory () in
    let* workspace = scan_workspace common in
    init_build_system workspace
      ~sandboxing_preference:(Common.config common).sandboxing_preference
      ?memory ?external_lib_deps_mode ?only_packages
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

let do_build (setup : Main.build_system) targets =
  Build_system.do_build ~request:(Target.request setup targets)

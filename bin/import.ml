open Stdune
open Dune

module Term       = Cmdliner.Term
module Manpage    = Cmdliner.Manpage

module Super_context  = Dune.Super_context
module Context        = Dune.Context
module Config         = Dune.Config
module Lib_name       = Dune.Lib_name
module Lib_deps_info  = Dune.Lib_deps_info
module Build_system   = Dune.Build_system
module Findlib        = Dune.Findlib
module Package        = Dune.Package
module Dune_package   = Dune.Dune_package
module Hooks          = Dune.Hooks
module Build          = Dune.Build
module Action         = Dune.Action
module Dep            = Dune.Dep
module Action_to_sh   = Dune.Action_to_sh
module Dpath = Dune.Dpath
module Install        = Dune.Install
module Watermarks     = Dune.Watermarks
module Promotion      = Dune.Promotion
module Colors         = Dune.Colors
module Report_error   = Dune.Report_error
module Dune_project   = Dune.Dune_project
module Workspace      = Dune.Workspace
module Cached_digest  = Dune.Cached_digest

include Common.Let_syntax

module Main = struct

  include Dune.Main

  let scan_workspace ~log (common : Common.t) =
    scan_workspace
      ~log
      ?workspace_file:(Option.map ~f:Arg.Path.path common.workspace_file)
      ?x:common.x
      ?profile:common.profile
      ~capture_outputs:common.capture_outputs
      ~ancestor_vcs:common.root.ancestor_vcs
      ()

  let setup ~log ?external_lib_deps_mode (common : Common.t) =
    let open Fiber.O in
    scan_workspace ~log common
    >>= init_build_system
          ?external_lib_deps_mode
          ?only_packages:common.only_packages
end

module Log = struct
  include Stdune.Log

  let create (common : Common.t) =
    Log.create ~display:common.config.display ()
end

module Scheduler = struct
  include Dune.Scheduler
  open Fiber.O

  let go ?log ~(common : Common.t) f =
    let f () =
      Main.set_concurrency ?log common.config >>= f
    in
    Scheduler.go ?log ~config:common.config f

  let poll ?log ~(common : Common.t) ~once ~finally () =
    let once () =
      let* () = Main.set_concurrency ?log common.config in
      once ()
    in
    Scheduler.poll ?log ~config:common.config ~once ~finally ()
end

let restore_cwd_and_execve (common : Common.t) prog argv env =
  let prog =
    if Filename.is_relative prog then
      Filename.concat common.root.dir prog
    else
      prog
  in
  Proc.restore_cwd_and_execve prog argv ~env

let do_build (setup : Main.build_system) targets =
  Build_system.do_build
    ~request:(Target.request setup targets)

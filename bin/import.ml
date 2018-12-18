open Stdune
open Dune

(* Things in src/ don't depend on cmdliner to speed up the
   bootstrap, so we set this reference here *)
let () = Import.suggest_function := Cmdliner_suggest.value

module Term       = Cmdliner.Term
module Manpage    = Cmdliner.Manpage
module Let_syntax = Cmdliner.Term

module Super_context  = Dune.Super_context
module Context        = Dune.Context
module Config         = Dune.Config
module Lib_name       = Dune.Lib_name
module Lib_deps_info  = Dune.Lib_deps_info
module Build_system   = Dune.Build_system
module Findlib        = Dune.Findlib
module Package        = Dune.Package
module Dune_package   = Dune.Dune_package
module Utils          = Dune.Utils
module Hooks          = Dune.Hooks
module Build          = Dune.Build
module Action         = Dune.Action
module Deps           = Dune.Deps
module Action_to_sh   = Dune.Action_to_sh
module Path_dune_lang = Dune.Path_dune_lang
module Install        = Dune.Install
module Watermarks     = Dune.Watermarks
module Promotion      = Dune.Promotion
module Colors         = Dune.Colors
module Report_error   = Dune.Report_error
module Dune_project   = Dune.Dune_project
module Workspace      = Dune.Workspace

let die = Dune.Import.die
let hint = Dune.Import.hint

module Main = struct
  include Dune.Main

  let setup ~log ?external_lib_deps_mode (common : Common.t) =
    setup
      ~log
      ?workspace_file:(Option.map ~f:Arg.Path.path common.workspace_file)
      ?only_packages:common.only_packages
      ?external_lib_deps_mode
      ?x:common.x
      ?profile:common.profile
      ~ignore_promoted_rules:common.ignore_promoted_rules
      ~capture_outputs:common.capture_outputs
      ()
end

module Log = struct
  include Dune.Log

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
      Main.set_concurrency ?log common.config
      >>= fun () ->
      once ()
    in
    Scheduler.poll ?log ~config:common.config ~once ~finally ()
end

let restore_cwd_and_execve (common : Common.t) prog argv env =
  let prog =
    if Filename.is_relative prog then
      Filename.concat common.root prog
    else
      prog
  in
  Proc.restore_cwd_and_execve prog argv ~env

let do_build (setup : Main.setup) targets =
  Build_system.do_build setup.build_system
    ~request:(Target.request setup targets)

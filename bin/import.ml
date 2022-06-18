open Stdune
open Dune_engine
module Term = Cmdliner.Term
module Manpage = Cmdliner.Manpage
module Stanza = Dune_lang.Stanza
module Super_context = Dune_rules.Super_context
module Context = Dune_rules.Context
module Config = Dune_util.Config
module Lib_name = Dune_rules.Lib_name
module Build_config = Dune_engine.Build_config
module Build_system = Dune_engine.Build_system
module Load_rules = Dune_engine.Load_rules
module Findlib = Dune_rules.Findlib
module Package = Dune_engine.Package
module Dune_package = Dune_rules.Dune_package
module Hooks = Dune_engine.Hooks
module Action_builder = Dune_engine.Action_builder
module Action = Dune_engine.Action
module Dep = Dune_engine.Dep
module Action_to_sh = Dune_engine.Action_to_sh
module Dpath = Dune_engine.Dpath
module Install = Dune_rules.Install
module Section = Dune_engine.Section
module Diff_promotion = Dune_engine.Diff_promotion
module Colors = Dune_rules.Colors
module Dune_project = Dune_engine.Dune_project
module Workspace = Dune_rules.Workspace
module Cached_digest = Dune_engine.Cached_digest
module Targets = Dune_engine.Targets
module Profile = Dune_rules.Profile
module Resolve = Dune_rules.Resolve
module Log = Dune_util.Log
module Dune_rpc = Dune_rpc_private
module Graph = Dune_graph.Graph
include Common.Let_syntax

let in_group (t, info) = (Term.Group.Term t, info)

module Main : sig
  include module type of struct
    include Dune_rules.Main
  end

  val setup : unit -> build_system Memo.t Fiber.t
end = struct
  include Dune_rules.Main

  let setup () =
    let open Fiber.O in
    let* scheduler = Scheduler.t () in
    Console.Status_line.set
      (Live
         (fun () ->
           match Fiber.Svar.read Build_system.state with
           | Initializing
           | Restarting_current_build
           | Build_succeeded__now_waiting_for_changes
           | Build_failed__now_waiting_for_changes -> Pp.nop
           | Building
               { Build_system.Progress.number_of_rules_executed = done_
               ; number_of_rules_discovered = total
               } ->
             Pp.verbatim
               (sprintf "Done: %u%% (%u/%u, %u left) (jobs: %u)"
                  (if total = 0 then 0 else done_ * 100 / total)
                  done_ total (total - done_)
                  (Scheduler.running_jobs_count scheduler))));
    Fiber.return (Memo.of_thunk get)
end

module Scheduler = struct
  include Dune_engine.Scheduler

  let maybe_clear_screen ~details_hum (dune_config : Dune_config.t) =
    match Dune_util.Config.inside_dune with
    | true -> (* Don't print anything here to make tests less verbose *) ()
    | false -> (
      match dune_config.terminal_persistence with
      | Clear_on_rebuild -> Console.reset ()
      | Preserve ->
        let message =
          sprintf "********** NEW BUILD (%s) **********"
            (String.concat ~sep:", " details_hum)
        in
        Console.print_user_message
          (User_message.make
             [ Pp.nop
             ; Pp.tag User_message.Style.Success (Pp.verbatim message)
             ; Pp.nop
             ]))

  let on_event dune_config _config = function
    | Scheduler.Run.Event.Tick -> Console.Status_line.refresh ()
    | Source_files_changed { details_hum } ->
      maybe_clear_screen ~details_hum dune_config
    | Build_interrupted ->
      Console.Status_line.set
        (Live
           (fun () ->
             let progression =
               match Fiber.Svar.read Build_system.state with
               | Initializing
               | Restarting_current_build
               | Build_succeeded__now_waiting_for_changes
               | Build_failed__now_waiting_for_changes ->
                 { Build_system.Progress.number_of_rules_discovered = 0
                 ; number_of_rules_executed = 0
                 }
               | Building progress -> progress
             in
             Pp.seq
               (Pp.tag User_message.Style.Error
                  (Pp.verbatim "Source files changed"))
               (Pp.verbatim
                  (sprintf ", restarting current build... (%u/%u)"
                     progression.number_of_rules_executed
                     progression.number_of_rules_discovered))))
    | Build_finish build_result ->
      let message =
        match build_result with
        | Success -> Pp.tag User_message.Style.Success (Pp.verbatim "Success")
        | Failure -> Pp.tag User_message.Style.Error (Pp.verbatim "Had errors")
      in
      Console.Status_line.set
        (Constant
           (Pp.seq message (Pp.verbatim ", waiting for filesystem changes...")))

  let go ~(common : Common.t) ~config:dune_config f =
    let stats = Common.stats common in
    let config = Dune_config.for_scheduler dune_config stats in
    Scheduler.Run.go config ~on_event:(on_event dune_config) f

  let go_with_rpc_server_and_console_status_reporting ~(common : Common.t)
      ~config:dune_config run =
    let stats = Common.stats common in
    let config = Dune_config.for_scheduler dune_config stats in
    let file_watcher = Common.file_watcher common in
    let rpc = Common.rpc common in
    let run () =
      Fiber.fork_and_join_unit (fun () -> Dune_rpc_impl.Server.run rpc) run
    in
    Scheduler.Run.go config ~file_watcher ~on_event:(on_event dune_config) run
end

let restore_cwd_and_execve (common : Common.t) prog argv env =
  let prog =
    if Filename.is_relative prog then
      let root = Common.root common in
      Filename.concat root.dir prog
    else prog
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

open Stdune

include struct
  open Dune_engine
  module Build_config = Build_config
  module Build_system = Build_system
  module Load_rules = Load_rules
  module Package = Package
  module Hooks = Hooks
  module Action_builder = Action_builder
  module Action = Action
  module Dep = Dep
  module Action_to_sh = Action_to_sh
  module Dpath = Dpath
  module Findlib = Dune_rules.Findlib
  module Dune_package = Dune_rules.Dune_package
  module Install = Dune_rules.Install
  module Section = Section
  module Diff_promotion = Diff_promotion
  module Dune_project = Dune_project
  module Cached_digest = Cached_digest
  module Targets = Targets
end

include struct
  open Dune_rules
  module Super_context = Super_context
  module Context = Context
  module Config = Dune_util.Config
  module Lib_name = Lib_name
  module Workspace = Workspace
  module Profile = Profile
  module Resolve = Resolve
end

include struct
  open Cmdliner
  module Term = Term
  module Manpage = Manpage
  module Cmd = Cmd
end

module Digest = Dune_digest
module Metrics = Dune_metrics
module Console = Dune_console
module Stanza = Dune_lang.Stanza
module Log = Dune_util.Log
module Dune_rpc = Dune_rpc_private
module Graph = Dune_graph.Graph
include Common.Let_syntax

module Main : sig
  include module type of struct
    include Dune_rules.Main
  end

  val setup : unit -> build_system Memo.t Fiber.t
end = struct
  include Dune_rules.Main

  let setup () =
    let open Fiber.O in
    let* scheduler = Dune_engine.Scheduler.t () in
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
                  (Dune_engine.Scheduler.running_jobs_count scheduler))));
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
      | Clear_on_rebuild_and_flush_history -> Console.reset_flush_history ()
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
    | Run.Event.Tick -> Console.Status_line.refresh ()
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
                 Build_system.Progress.init
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
    let config =
      let insignificant_changes = Common.insignificant_changes common in
      Dune_config.for_scheduler dune_config stats ~insignificant_changes
        ~signal_watcher:`Yes
    in
    Run.go config ~on_event:(on_event dune_config) f

  let go_with_rpc_server_and_console_status_reporting ~(common : Common.t)
      ~config:dune_config run =
    let stats = Common.stats common in
    let config =
      let insignificant_changes = Common.insignificant_changes common in
      Dune_config.for_scheduler dune_config stats ~insignificant_changes
        ~signal_watcher:`Yes
    in
    let file_watcher = Common.file_watcher common in
    let rpc = Common.rpc common in
    let run () =
      Fiber.fork_and_join_unit (fun () -> Dune_rpc_impl.Server.run rpc) run
    in
    Run.go config ~file_watcher ~on_event:(on_event dune_config) run
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
let command_alias cmd term name =
  let orig = Cmd.name cmd in
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
  Cmd.v (Cmd.info name ~docs:"COMMAND ALIASES" ~doc ~man) term

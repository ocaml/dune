include Stdune
include Dune_config_file
include Dune_vcs

include struct
  open Dune_engine
  module Build_config = Build_config
  module Build_system = Build_system
  module Build_system_error = Build_system_error
  module Load_rules = Load_rules
  module Hooks = Hooks
  module Action_builder = Dune_rules.Action_builder
  module Action = Action
  module Dep = Dep
  module Action_to_sh = Action_to_sh
  module Dpath = Dpath
  module Findlib = Dune_rules.Findlib
  module Diff_promotion = Diff_promotion
  module Targets = Targets
  module Context_name = Context_name
end

module Cached_digest = Dune_digest.Cached_digest
module Execution_env = Dune_util.Execution_env

include struct
  open Dune_rules
  module Super_context = Super_context
  module Context = Context
  module Workspace = Workspace
  module Package = Package
  module Dune_project = Dune_project
  module Dune_project_name = Dune_project_name
  module Dune_package = Dune_package
  module Resolve = Resolve
  module Source_dir_status = Source_dir_status
  module Source_tree = Source_tree
  module Dune_file = Dune_file
  module Library = Library
  module Melange = Melange
  module Melange_stanzas = Melange_stanzas
  module Executables = Executables
end

include struct
  open Cmdliner
  module Term = Term
  module Manpage = Manpage

  module Cmd = struct
    include Cmd

    let default_exits = List.map ~f:Exit_code.info Exit_code.all

    let info ?docs ?doc ?man ?envs ?version name =
      info ?docs ?doc ?man ?envs ?version ~exits:default_exits name
    ;;
  end
end

module Digest = Dune_digest
module Metrics = Dune_metrics
module Console = Dune_console

include struct
  open Dune_lang
  module Stanza = Stanza
  module Profile = Profile
  module Lib_name = Lib_name
  module Package_name = Package_name
  module Package_version = Package_version
  module Source_kind = Source_kind
  module Package_info = Package_info
  module Section = Section
end

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
                  (Dune_engine.Scheduler.running_jobs_count scheduler))));
    Fiber.return (Memo.of_thunk get)
  ;;
end

module Scheduler = struct
  include Dune_engine.Scheduler

  let maybe_clear_screen ~details_hum (dune_config : Dune_config.t) =
    match Execution_env.inside_dune with
    | true -> (* Don't print anything here to make tests less verbose *) ()
    | false ->
      (match dune_config.terminal_persistence with
       | Clear_on_rebuild -> Console.reset ()
       | Clear_on_rebuild_and_flush_history -> Console.reset_flush_history ()
       | Preserve ->
         let message =
           sprintf
             "********** NEW BUILD (%s) **********"
             (String.concat ~sep:", " details_hum)
         in
         Console.print_user_message
           (User_message.make
              [ Pp.nop; Pp.tag User_message.Style.Success (Pp.verbatim message); Pp.nop ]))
  ;;

  let on_event dune_config _config = function
    | Run.Event.Tick -> Console.Status_line.refresh ()
    | Source_files_changed { details_hum } -> maybe_clear_screen ~details_hum dune_config
    | Build_interrupted ->
      Console.Status_line.set
        (Live
           (fun () ->
             let progression =
               match Fiber.Svar.read Build_system.state with
               | Initializing
               | Restarting_current_build
               | Build_succeeded__now_waiting_for_changes
               | Build_failed__now_waiting_for_changes -> Build_system.Progress.init
               | Building progress -> progress
             in
             Pp.seq
               (Pp.tag User_message.Style.Error (Pp.verbatim "Source files changed"))
               (Pp.verbatim
                  (sprintf
                     ", restarting current build... (%u/%u)"
                     progression.number_of_rules_executed
                     progression.number_of_rules_discovered))))
    | Build_finish build_result ->
      let message =
        match build_result with
        | Success -> Pp.tag User_message.Style.Success (Pp.verbatim "Success")
        | Failure ->
          let failure_message =
            match
              Build_system_error.(
                Id.Map.cardinal (Set.current (Fiber.Svar.read Build_system.errors)))
            with
            | 1 -> Pp.textf "Had 1 error"
            | n -> Pp.textf "Had %d errors" n
          in
          Pp.tag User_message.Style.Error failure_message
      in
      Console.Status_line.set
        (Constant (Pp.seq message (Pp.verbatim ", waiting for filesystem changes...")))
  ;;

  let rpc server =
    { Dune_engine.Rpc.run = Dune_rpc_impl.Server.run server
    ; stop = Dune_rpc_impl.Server.stop server
    ; ready = Dune_rpc_impl.Server.ready server
    }
  ;;

  let go ~(common : Common.t) ~config:dune_config f =
    let stats = Common.stats common in
    let config =
      let watch_exclusions = Common.watch_exclusions common in
      Dune_config.for_scheduler
        dune_config
        stats
        ~print_ctrl_c_warning:true
        ~watch_exclusions
    in
    let f =
      match Common.rpc common with
      | `Allow server -> fun () -> Dune_engine.Rpc.with_background_rpc (rpc server) f
      | `Forbid_builds -> f
    in
    Run.go config ~on_event:(on_event dune_config) f
  ;;

  let go_with_rpc_server_and_console_status_reporting
    ~(common : Common.t)
    ~config:dune_config
    run
    =
    let server =
      match Common.rpc common with
      | `Allow server -> rpc server
      | `Forbid_builds -> Code_error.raise "rpc must be enabled in polling mode" []
    in
    let stats = Common.stats common in
    let config =
      let watch_exclusions = Common.watch_exclusions common in
      Dune_config.for_scheduler
        dune_config
        stats
        ~print_ctrl_c_warning:true
        ~watch_exclusions
    in
    let file_watcher = Common.file_watcher common in
    let run () =
      let open Fiber.O in
      Dune_engine.Rpc.with_background_rpc server
      @@ fun () ->
      let* () = Dune_engine.Rpc.ensure_ready () in
      run ()
    in
    Run.go config ~file_watcher ~on_event:(on_event dune_config) run
  ;;
end

let restore_cwd_and_execve (common : Common.t) prog argv env =
  let prog =
    if Filename.is_relative prog
    then (
      let root = Common.root common in
      Filename.concat root.dir prog)
    else prog
  in
  Proc.restore_cwd_and_execve prog argv ~env
;;

(* Adapted from
   https://github.com/ocaml/opam/blob/fbbe93c3f67034da62d28c8666ec6b05e0a9b17c/src/client/opamArg.ml#L759 *)
let command_alias ?orig_name cmd term name =
  let orig =
    match orig_name with
    | Some s -> s
    | None -> Cmd.name cmd
  in
  let doc = Printf.sprintf "An alias for $(b,%s)." orig in
  let man =
    [ `S "DESCRIPTION"
    ; `P (Printf.sprintf "$(mname)$(b, %s) is an alias for $(mname)$(b, %s)." name orig)
    ; `P (Printf.sprintf "See $(mname)$(b, %s --help) for details." orig)
    ; `Blocks Common.help_secs
    ]
  in
  Cmd.v (Cmd.info name ~docs:"COMMAND ALIASES" ~doc ~man) term
;;

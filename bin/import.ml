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

include struct
  open Source
  module Source_tree = Source_tree
  module Source_dir_status = Source_dir_status
  module Workspace = Workspace
end

include struct
  open Dune_rules
  module Super_context = Super_context
  module Context = Context
  module Dune_package = Dune_package
  module Resolve = Resolve
  module Dune_file = Dune_file
  module Library = Library
  module Melange = Melange
  module Executables = Executables
end

include struct
  open Cmdliner
  module Term = Term
  module Manpage = Manpage

  module Cmd = struct
    include Cmd

    let default_exits = List.map ~f:Exit_code.info Exit_code.all

    let info ?docs ?doc ?man_xrefs ?man ?envs ?version name =
      info ?docs ?doc ?man_xrefs ?man ?envs ?version ~exits:default_exits name
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
  module Package = Package
  module Package_version = Package_version
  module Source_kind = Source_kind
  module Package_info = Package_info
  module Section = Section
  module Dune_project_name = Dune_project_name
  module Dune_project = Dune_project
end

include struct
  open Dune_pkg
  module Opam_repo = Opam_repo
  module Lock_dir = Lock_dir
  module Rev_store = Rev_store
  module Resolved_package = Resolved_package
end

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

let string_path_relative_to_specified_root (root : Workspace_root.t) path =
  if Filename.is_relative path then Filename.concat root.dir path else path
;;

let restore_cwd_and_execve root prog args env =
  let prog = string_path_relative_to_specified_root root prog in
  Proc.restore_cwd_and_execve prog args ~env
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

(* The build system has some global state which makes it unsafe for
   multiple instances of it to be executed concurrently, so we ensure
   serialization by holding this mutex while running the build system. *)
let build_system_mutex = Fiber.Mutex.create ()

let build f =
  Hooks.End_of_build.once Promote.Diff_promotion.finalize;
  Fiber.Mutex.with_lock build_system_mutex ~f:(fun () -> Build_system.run f)
;;

let build_exn f =
  Hooks.End_of_build.once Promote.Diff_promotion.finalize;
  Fiber.Mutex.with_lock build_system_mutex ~f:(fun () -> Build_system.run_exn f)
;;

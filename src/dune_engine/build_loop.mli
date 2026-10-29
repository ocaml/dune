open Import

(** State shared by watch-mode build loops. *)
type t

val create : unit -> t

(** [run t f] initializes watch-mode file-watcher state and runs [f]. *)
val run : t -> (unit -> 'a Fiber.t) -> 'a Fiber.t

(** [submit_rpc_request t ~session_id ~request_id ~build] adds a one-shot RPC
    build request. If a build is already running, it is cancelled and restarted
    with [build] included in the combined build request. *)
val submit_rpc_request
  :  t
  -> session_id:Rpc.Server.Session.Id.t
  -> request_id:Dune_rpc.Id.t
  -> build:unit Action_builder.t
  -> Build_outcome.t Fiber.t

(** [goals_count t] is the number of goals in the build currently of [t], if any.

    The watch mode sticky goal is counted if it is part of the build. *)
val goals_count : t -> int

(** The run of the build currently running, if any. *)
val current_run_id : t -> Run_id.t option

(** When the build currently running started, if any. *)
val current_build_started_at : t -> Time.t option

(** The status line section for the timing of the build [run_id], building
    [goals] goals. *)
val build_timing_status
  :  Run_id.t
  -> goals:int
  -> build_duration:Time.Span.t
  -> action_duration:Time.Span.t
  -> _ Pp.t

val cancel_rpc_requests_by_session
  :  t
  -> session_id:Rpc.Server.Session.Id.t
  -> unit Fiber.t

val cancel_all_rpc_requests : t -> unit Fiber.t

(** [flush_file_watcher t] waits until file-watcher notifications that precede
    the flush have been applied to [t], including any active debounce quiet
    period. *)
val flush_file_watcher : t -> unit Fiber.t

(** Simulate a file-watcher queue overflow. This is exposed for testing through
    Dune's private RPC protocol. *)
val simulate_file_watcher_queue_overflow : t -> unit Fiber.t

(** [poll t ~action_runner ~sticky_goal] runs the watch loop managed by [t].
    [action_runner] is used for each build started by the loop. [sticky_goal] is
    rebuilt after file changes and is also included in builds started for
    one-shot RPC requests. *)
val poll
  :  t
  -> action_runner:Action_runner.t option
  -> sticky_goal:unit Action_builder.t option
  -> unit Fiber.t

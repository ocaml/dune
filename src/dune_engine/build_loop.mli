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

val cancel_rpc_requests_by_session
  :  t
  -> session_id:Rpc.Server.Session.Id.t
  -> unit Fiber.t

val cancel_all_rpc_requests : t -> unit Fiber.t

(** [poll t ~action_runner ~sticky_goal] runs the watch loop managed by [t].
    [action_runner] is used for each build started by the loop. [sticky_goal] is
    rebuilt after file changes and is also included in builds started for
    one-shot RPC requests. *)
val poll
  :  t
  -> action_runner:Action_runner.t option
  -> sticky_goal:unit Action_builder.t option
  -> unit Fiber.t

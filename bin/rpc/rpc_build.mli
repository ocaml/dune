open! Import

(** Sends a command to an RPC server to build the specified targets and wait
    for the build to complete or fail. If [wait] is true then wait until an RPC
    server is running before making the request. Otherwise if no RPC server is
    running then raise a [User_error].  *)
val build
  :  wait:bool
  -> Common.Builder.t
  -> Dune_util.Global_lock.Lock_held_by.t
  -> Dune_lang.Dep_conf.t list
  -> (Dune_rpc.Build_outcome_with_diagnostics.t, Dune_rpc.Response.Error.t) result Fiber.t

(** dune rpc build command *)
val cmd : unit Cmdliner.Cmd.t

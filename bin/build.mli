open Import

(** Connect to an RPC server (waiting for the server to start if necessary) and
    then send a request to the server to build the specified targets. If the
    build fails then a diagnostic error message is printed. If
    [print_on_success] is true then this function will also print a message
    after the build succeeds. *)
val build_via_rpc_server
  :  print_on_success:bool
  -> targets:Dune_lang.Dep_conf.t list
  -> Common.Builder.t
  -> Dune_util.Global_lock.Lock_held_by.t
  -> unit Fiber.t

val run_build_system
  :  common:Common.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> (unit, [ `Already_reported ]) result Fiber.t

val build : unit Cmd.t

val run_build_command
  :  common:Common.t
  -> config:Dune_config.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> unit

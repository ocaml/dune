open Import

val run_build_system
  :  request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> (unit, [ `Already_reported ]) result Fiber.t

val build : unit Cmd.t

val run_build_command
  :  common:Common.t
  -> config:Dune_config.t
  -> request:(Dune_rules.Main.build_system -> unit Action_builder.t)
  -> unit

val build_memo : (unit -> 'a Memo.t) -> ('a, [ `Already_reported ]) Result.t Fiber.t
val build_memo_exn : (unit -> 'a Memo.t) -> 'a Fiber.t

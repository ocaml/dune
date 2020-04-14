(** Collect stats during the execution of dune *)

val enable : string -> unit
(** Enable stats recording *)

val record : unit -> unit
(** If stats recording is enabled, collect stats now *)

val with_process :
  program:string -> args:string list -> 'a Fiber.t -> 'a Fiber.t
(** Collect data about a subprocess *)

val new_evaluated_rule : unit -> unit
(** Called by the build system when a new rule is fully evaluated and ready to
    fire *)

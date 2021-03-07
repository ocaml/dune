(** Collect stats during the execution of dune *)

(** Collect data about a subprocess *)
val with_process :
  program:string -> args:string list -> 'a Fiber.t -> 'a Fiber.t

(** Called by the build system when a new rule is fully evaluated and ready to
    fire *)
val new_evaluated_rule : unit -> unit

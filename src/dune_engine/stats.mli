(** Collect stats during the execution of dune *)

(** Collect data about a subprocess *)
val with_process :
  program:string -> args:string list -> 'a Fiber.t -> 'a Fiber.t

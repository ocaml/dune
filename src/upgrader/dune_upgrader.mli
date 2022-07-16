(** Upgrade projects from jbuilder to Dune *)

(** Upgrade all projects in this file tree *)
val upgrade : unit -> unit Fiber.t

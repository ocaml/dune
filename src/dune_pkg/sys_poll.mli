(** Functions to retrieve the values of system-dependent OPAM values like "arch"
    or "os". *)
open Import

type t

val make : path:Path.t list -> t

(** Returns the value of [arch] *)
val arch : t -> string option Fiber.t

(** Returns the value of [os] *)
val os : t -> string option Fiber.t

(** Returns the value of [os-version] *)
val os_version : t -> string option Fiber.t

(** Returns the value of [os-distribution] *)
val os_distribution : t -> string option Fiber.t

(** Returns the value of [os-family] *)
val os_family : t -> string option Fiber.t

(** Returns the value of [sys-ocaml-version] *)
val sys_ocaml_version : t -> string option Fiber.t

(** Returns a solver environment where all the system-dependent values that
    could be retrieved are set *)
val solver_env_from_current_system : t -> Solver_env.t Fiber.t

(** Functions to retrieve the values of system-dependent OPAM values like "arch"
    or "os". *)
open Import

(** Returns the value of [arch] *)
val arch : path:Path.t list -> string option Fiber.t

(** Returns the value of [os] *)
val os : path:Path.t list -> string option Fiber.t

(** Returns the value of [os-version] *)
val os_version : path:Path.t list -> string option Fiber.t

(** Returns the value of [os-distribution] *)
val os_distribution : path:Path.t list -> string option Fiber.t

(** Returns the value of [os-family] *)
val os_family : path:Path.t list -> string option Fiber.t

(** Returns the value of [sys-ocaml-version] *)
val sys_ocaml_version : path:Path.t list -> string option Fiber.t

(** Returns a solver environment where all the system-dependent values that
    could be retrieved are set *)
val solver_env_from_current_system : path:Path.t list -> Solver_env.t Fiber.t

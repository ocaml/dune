open Import

(** creates a lock file at the specified location(s) *)
val lock
  :  version_preference:Dune_pkg.Version_preference.t option
  -> lock_dirs_arg:Pkg_common.Lock_dirs_arg.t
  -> unit Fiber.t

(** Command to create lock directory *)
val command : unit Cmd.t

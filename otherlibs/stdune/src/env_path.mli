(** Handle the [PATH] environment variable. *)

(* this isn't in [Env] to avoid cycles *)

val var : Env.Var.t

val cons : Env.t -> dir:Path.t -> Env.t

val path : Env.t -> Path.t list

(** Handle the [PATH] environment variable. *)

(* this isn't in [Env] to avoid cycles *)

val var : Env.Var.t

(** [cons env ~dir] adds [dir] to the start of the PATH variable in [env] *)
val cons : ?var:Env.Var.t -> Env.t -> dir:Path.t -> Env.t

val path : Env.t -> Path.t list

(** [extend_env_concat_path a b] adds all variables from [b] to [a]
    overwriting any existing values of those variables in [a] except for PATH
    which is set to the concatenation of the PATH variables from [a] and [b]
    with the PATH entries from [b] preceding the PATH entries from [a] *)
val extend_env_concat_path : Env.t -> Env.t -> Env.t

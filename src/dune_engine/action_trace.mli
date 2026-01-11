open Import

type t

val add_to_env : t -> Env.t -> Env.t
val create : Dune_digest.t -> t
val collect : t -> unit

open Import

type t = private A of string [@@unboxed]

val equal : t -> t -> bool

val is_valid : string -> bool

val of_string : string -> t

val to_string : t -> string

(** [parse s] is [Some (a:t)] if [s] can be a valid atom according to [is_valid]
    otherwise it is [None] *)
val parse : string -> t option

val of_int : int -> t

val of_float : float -> t

val of_bool : bool -> t

val of_int64 : Int64.t -> t

val of_digest : Digest.t -> t

val to_dyn : t -> Dyn.t

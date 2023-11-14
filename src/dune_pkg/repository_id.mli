open Import

(** [t] represents an identifier of the state of an opam repository that can
    potentially be used to reproduce the lock dir/lock file exactly. *)
type t

val encode : t Encoder.t
val decode : t Decoder.t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val of_path : Path.t -> t option
val git_hash : t -> string option
val of_git_hash : string -> t

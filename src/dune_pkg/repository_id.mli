open Import

(** [t] represents an identifier of the state of an opam repository that can
    potentially be used to reproduce the lock dir/lock file exactly. *)
type t

val encode : t -> Dune_sexp.t
val decode : t Dune_sexp.Decoder.t
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val of_path : Path.t -> t option

module Private : sig
  val git_hash : string -> t
end

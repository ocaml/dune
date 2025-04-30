(** This is a temporary partial copy of the old [Dune_digest] module based on MD5. It is
    used in package management to compute checksums and will be replaced accordingly in
    the future. *)
open Import

(** Digests (MD5) *)
type t

val to_dyn : t -> Dyn.t
val equal : t -> t -> bool
val to_string : t -> string
val from_hex : string -> t option
val file : Path.t -> t
val string : string -> t

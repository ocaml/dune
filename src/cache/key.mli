open Stdune

(** Cache keys are currently MD5 digests of the corresponding build rules. *)
type t = Digest.t

val of_string : string -> (t, string) Result.t

val to_string : t -> string

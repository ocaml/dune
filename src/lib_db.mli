(** Where libraries are *)

type t

val create : Findlib.t -> (Path.t * Jbuild_types.Stanza.t list) list -> t

val find : t -> string -> Lib.t

val top_closure : t -> string list -> Lib.t list

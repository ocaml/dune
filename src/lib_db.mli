(** Where libraries are *)

type t

val create : Findlib.t -> (Path.t * Jbuild_types.Stanza.t list) list -> t

val find : t -> string -> Lib.t

val find_internal : t -> string -> Lib.Internal.t option
val split : t -> string list -> Lib.Internal.t list * string list

(*val top_closure : t -> string list -> Lib.t list*)

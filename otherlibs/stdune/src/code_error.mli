(** A programming error that should be reported upstream *)

type t =
  { message : string
  ; data : (string * Dyn.t) list
  ; loc : Loc0.t option
  }

exception E of t

val to_dyn_without_loc : t -> Dyn.t
val to_dyn : t -> Dyn.t
val create : ?loc:Loc0.t -> string -> (string * Dyn.t) list -> t
val raise : ?loc:Loc0.t -> string -> (string * Dyn.t) list -> _

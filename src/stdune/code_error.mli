(** A programming error that should be reported upstream *)

type t =
  { message : string
  ; data : (string * Dyn.t) list
  }

exception E of t

val to_dyn : t -> Dyn.t

val raise : string -> (string * Dyn.t) list -> _

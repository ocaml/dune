(** A programming error that should be reported upstream *)

type t =
  { message : string
  ; data : (string * Dyn0.t) list
  }

exception E of t

val to_dyn : t -> Dyn0.t

val raise : string -> (string * Dyn0.t) list -> _

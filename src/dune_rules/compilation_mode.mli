open Import

type t =
  | Ocaml
  | Melange

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t

type modes =
  { modes : t list
  ; merlin : t
  }

val modes : Lib_mode.Map.Set.t -> modes

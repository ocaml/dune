(** Uninhabited type *)

type t = (int, string) Type_eq.t

val unreachable_code : t -> 'a

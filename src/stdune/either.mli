(** Left or right *)

type ('a, 'b) t =
  | Left  of 'a
  | Right of 'b

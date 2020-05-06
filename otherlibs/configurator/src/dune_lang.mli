(** Subset of dune_lang to print flag lists *)
type t =
  | Quoted_string of string
  | List of t list

val to_string : t -> string

open Import

type t

val get_words : t -> string -> Variant.Set.t -> string list
val get : t -> string -> Variant.Set.t -> string option
val to_dyn : t -> Dyn.t
val of_meta_rules : Meta.Simplified.Rules.t String.Map.t -> t
val empty : t

val union
  :  t
  -> t
  -> f:(string -> Findlib_rules.t -> Findlib_rules.t -> Findlib_rules.t option)
  -> t

val to_string_map : t -> f:(Findlib_rules.t -> string option) -> string String.Map.t

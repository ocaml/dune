type t

val of_meta_rules : Meta.Simplified.Rules.t -> t
val interpret : t -> preds:Ocaml.Variant.Set.t -> string option
val to_dyn : t -> Dyn.t
val union : t -> t -> t

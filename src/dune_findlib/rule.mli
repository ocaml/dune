open Import

type t =
  { preds_required : Variant.Set.t
  ; preds_forbidden : Variant.Set.t
  ; value : string
  }

val to_dyn : t -> Dyn.t
val matches : t -> preds:Variant.Set.t -> bool
val formal_predicates_count : t -> int
val make : Meta.rule -> t

open Import
module P = Ocaml.Variant
module Ps = Ocaml.Variant.Set
(* An assignment or addition *)

type t =
  { preds_required : Ps.t
  ; preds_forbidden : Ps.t
  ; value : string
  }

let repr =
  Repr.record
    "findlib-rule"
    [ Repr.field "preds_required" (Repr.abstract Ps.to_dyn) ~get:(fun t ->
        t.preds_required)
    ; Repr.field "preds_forbidden" (Repr.abstract Ps.to_dyn) ~get:(fun t ->
        t.preds_forbidden)
    ; Repr.field "value" Repr.string ~get:(fun t -> t.value)
    ]
;;

let to_dyn = Repr.to_dyn repr

let formal_predicates_count t =
  Ps.cardinal t.preds_required + Ps.cardinal t.preds_forbidden
;;

let matches t ~preds =
  Ps.is_subset t.preds_required ~of_:preds
  && Ps.is_empty (Ps.inter preds t.preds_forbidden)
;;

let make (rule : Meta.rule) =
  let preds_required, preds_forbidden =
    List.partition_map rule.predicates ~f:(function
      | Pos x -> Left x
      | Neg x -> Right x)
  in
  { preds_required = Ps.of_list_map preds_required ~f:P.make
  ; preds_forbidden = Ps.of_list_map preds_forbidden ~f:P.make
  ; value = rule.value
  }
;;

open Import
module P = Ocaml.Variant
module Ps = Ocaml.Variant.Set
(* An assignment or addition *)

type t =
  { preds_required : Ps.t
  ; preds_forbidden : Ps.t
  ; value : string
  }

let to_dyn { preds_required; preds_forbidden; value } =
  let open Dyn in
  record
    [ "preds_required", Ps.to_dyn preds_required
    ; "preds_forbidden", Ps.to_dyn preds_forbidden
    ; "value", string value
    ]
;;

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

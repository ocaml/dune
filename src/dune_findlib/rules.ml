open Import

(* Set of rules for a given variable of a package. Implements the algorithm
   described here:

   http://projects.camlcity.org/projects/dl/findlib-1.6.3/doc/ref-html/r729.html *)

(* To implement the algorithm, [set_rules] is sorted by decreasing number of
   formal predicates, then according to the order of the META file.
   [add_rules] are in the same order as in the META file. *)
type t =
  { set_rules : Rule.t list
  ; add_rules : Rule.t list
  }

let to_dyn { set_rules; add_rules } =
  let open Dyn in
  record
    [ "set_rules", list Rule.to_dyn set_rules; "add_rules", list Rule.to_dyn add_rules ]
;;

let interpret t ~preds =
  let rec find_set_rule = function
    | [] -> None
    | rule :: rules ->
      if Rule.matches rule ~preds then Some rule.value else find_set_rule rules
  in
  let v = find_set_rule t.set_rules in
  List.fold_left t.add_rules ~init:v ~f:(fun v rule ->
    if Rule.matches rule ~preds
    then Some (Option.value ~default:"" v ^ " " ^ rule.value)
    else v)
;;

let sort =
  let compare a b =
    Int.compare (Rule.formal_predicates_count b) (Rule.formal_predicates_count a)
  in
  fun r -> List.stable_sort r ~compare
;;

let of_meta_rules (rules : Meta.Simplified.Rules.t) =
  let add_rules = List.map rules.add_rules ~f:Rule.make in
  let set_rules = List.map rules.set_rules ~f:Rule.make |> sort in
  { add_rules; set_rules }
;;

let union a b =
  { set_rules = a.set_rules @ b.set_rules |> sort; add_rules = a.add_rules @ b.add_rules }
;;

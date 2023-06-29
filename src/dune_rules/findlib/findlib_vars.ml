open Import
module Rules = Findlib_rules

type t = Rules.t String.Map.t

let get (t : t) var preds =
  Option.map (String.Map.find t var) ~f:(fun r ->
    Option.value ~default:"" (Rules.interpret r ~preds))
;;

let get_words t var preds =
  match get t var preds with
  | None -> []
  | Some s -> String.extract_comma_space_separated_words s
;;

let empty = String.Map.empty
let union = String.Map.union

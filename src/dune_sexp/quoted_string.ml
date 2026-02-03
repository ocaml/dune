open Stdune

module Block_kind = struct
  type t =
    | Escaped
    | Raw

  let delimiter = function
    | Escaped -> "\\|"
    | Raw -> "\\>"
  ;;

  let to_dyn = function
    | Escaped -> Dyn.variant "Escaped" []
    | Raw -> Dyn.variant "Raw" []
  ;;

  let equal = Poly.equal
end

type t =
  | Single of string
  | Multi of (Block_kind.t * Template.Part.t list) list

let to_string = function
  | Single s -> s
  | Multi lines ->
    String.concat
      ~sep:"\n"
      (List.map lines ~f:(fun (_, parts) -> Template.Part.list_to_string parts))
;;

let equal a b =
  match a, b with
  | Single s1, Single s2 -> String.equal s1 s2
  | Multi l1, Multi l2 ->
    List.equal (Tuple.T2.equal Block_kind.equal (List.equal Template.Part.equal)) l1 l2
  | Single _, Multi _ | Multi _, Single _ -> false
;;

let to_dyn = function
  | Single s -> Dyn.variant "Single" [ Dyn.string s ]
  | Multi lines ->
    Dyn.variant
      "Multi"
      [ Dyn.list (Dyn.pair Block_kind.to_dyn (Dyn.list Template.Part.to_dyn)) lines ]
;;

type flattened =
  | String of string
  | Parts of Template.Part.t list

let flatten = function
  | Single s -> String s
  | Multi lines ->
    let all_parts =
      List.concat
        (List.mapi lines ~f:(fun i (_, parts) ->
           if i = 0 then parts else Template.Part.Text "\n" :: parts))
    in
    let has_pforms =
      List.exists all_parts ~f:(function
        | Template.Part.Pform _ -> true
        | Template.Part.Text _ -> false)
    in
    if has_pforms
    then Parts all_parts
    else String (Template.Part.list_to_string all_parts)
;;

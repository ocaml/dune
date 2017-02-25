open! Import

type t = Sexp.Ast.t

let t t = t

let eval t ~special_values =
  let rec of_sexp : Sexp.Ast.t -> _ = function
    | Atom (loc, "\\") -> Loc.fail loc "unexpected \\"
    | Atom (loc, s) ->
      let len = String.length s in
      if len > 0 && s.[0] = ':' then
        let name = String.sub s ~pos:1 ~len:(len - 1) in
        match List.assoc name special_values with
        | l -> l
        | exception Not_found -> Loc.fail loc "undefined symbol %s" s;
      else
        [s]
    | List (_, sexps) -> of_sexps [] sexps
  and of_sexps acc = function
    | Atom (_, "\\") :: sexps -> of_sexps_negative acc sexps
    | elt :: sexps ->
      let elts = of_sexp elt in
      of_sexps (List.rev_append elts acc) sexps
    | [] -> List.rev acc
  and of_sexps_negative acc = function
    | Atom (_, "\\") :: sexps -> of_sexps_negative acc sexps
    | elt :: sexps ->
      let elts = of_sexp elt in
      let acc = List.filter acc ~f:(fun acc_elt -> not (List.mem acc_elt ~set:elts)) in
      of_sexps_negative acc sexps
    | [] -> List.rev acc
  in
  of_sexp t

let is_standard : t -> bool = function
  | Atom (_, ":standard") -> true
  | _ -> false

let eval_with_standard t ~standard =
  if is_standard t then
    standard (* inline common case *)
  else
    eval t ~special_values:[("standard", standard)]

let rec map (t : t) ~f =
  match t with
  | Atom (loc, s) ->
    let len = String.length s in
    if len > 0 && s.[0] = ':' then
      t
    else
      Atom (loc, f s)
  | List (loc, l) -> List (loc, List.map l ~f:(map ~f))

let standard : t = Atom (Loc.none, ":standard")

let append a b : t = List (Loc.none, [a; b])

module Unexpanded = struct
  type nonrec t = t
  let t t = t
  let standard = standard

  let append = append

  let files t =
    let rec loop acc : t -> _ = function
      | Atom _ -> acc
      | List (_, [Atom (_, ":include"); Atom (_, fn)]) -> String_set.add fn acc
      | List (_, l) -> List.fold_left l ~init:acc ~f:loop
    in
    loop String_set.empty t

  let rec expand (t : t) ~files_contents =
    match t with
    | Atom _ -> t
    | List (_, [Atom (_, ":include"); Atom (_, fn)]) ->
      String_map.find_exn fn files_contents ~string_of_key:(sprintf "%S")
        ~desc:(fun _ -> "<filename to s-expression>")
    | List (loc, l) -> List (loc, List.map l ~f:(expand ~files_contents))
end

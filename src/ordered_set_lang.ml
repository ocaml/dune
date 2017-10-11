open! Import

module Ast = struct
  [@@@warning "-37"]
  type expanded = Expanded
  type unexpanded = Unexpanded
  type ('a, _) t =
    | Element : 'a -> ('a, _) t
    | Special : Loc.t * string -> ('a, _) t
    | Union : ('a, 'b) t list -> ('a, 'b) t
    | Diff : ('a, 'b) t * ('a, 'b) t -> ('a, 'b) t
    | Include : String_with_vars.t -> ('a, unexpanded) t
end

type t = (string, Ast.expanded) Ast.t

let parse_general t ~f =
  let rec of_sexp : Sexp.Ast.t -> _ = function
    | Atom (loc, "\\") -> Loc.fail loc "unexpected \\"
    | Atom (_, "") as t -> Ast.Element (f t)
    | Atom (loc, s) as t ->
      if s.[0] = ':' then
        Special (loc, String.sub s ~pos:1 ~len:(String.length s - 1))
      else
        Element (f t)
    | List (_, sexps) -> of_sexps [] sexps
  and of_sexps acc = function
    | Atom (_, "\\") :: sexps -> Diff (Union (List.rev acc), of_sexps [] sexps)
    | elt :: sexps ->
      of_sexps (of_sexp elt :: acc) sexps
    | [] -> Union (List.rev acc)
  in
  of_sexp t

let t t : t = parse_general t ~f:(function Atom (_, s) -> s | List _ -> assert false)

let eval t ~special_values =
  let rec of_ast (t : t) =
    let open Ast in
    match t with
    | Element s -> [s]
    | Special (loc, name) ->
      begin
        match List.assoc name special_values with
        | l -> l
        | exception Not_found -> Loc.fail loc "undefined symbol %s" name;
      end
    | Union elts -> List.flatten (List.map elts ~f:of_ast)
    | Diff (left, right) ->
      let left = of_ast left in
      let right = of_ast right in
      List.filter left ~f:(fun acc_elt -> not (List.mem acc_elt ~set:right))
  in
  of_ast t

let is_standard : t -> bool = function
  | Ast.Special (_, "standard") -> true
  | _ -> false

let eval_with_standard t ~standard =
  if is_standard t then
    standard (* inline common case *)
  else
    eval t ~special_values:[("standard", standard)]

let rec map (t : t) ~f : t =
  let open Ast in
  match t with
  | Element s -> Element (f s)
  | Special _ -> t
  | Union l -> Union (List.map l ~f:(map ~f))
  | Diff (l, r) -> Diff (map l ~f, map r ~f)

let standard = Ast.Special (Loc.none, "standard")

let append a b = Ast.Union [a; b]

module Unexpanded = struct
  type t = (Sexp.Ast.t, Ast.unexpanded) Ast.t
  let t t =
    let rec map (t : (Sexp.Ast.t, Ast.expanded) Ast.t) =
      let open Ast in
      match t with
      | Element s -> Element s
      | Union [Special (_, "include"); Element fn] ->
        Include (String_with_vars.t fn)
      | Union [Special (loc, "include"); _]
      | Special (loc, "include") ->
        Loc.fail loc "(:include expects a single element (do you need to quote the filename?)"
      | Special (l, s) -> Special (l, s)
      | Union l ->
        Union (List.map l ~f:map)
      | Diff (l, r) ->
        Diff (map l, map r)
    in
    parse_general t ~f:(fun x -> x) |> map

  let standard = standard

  let append = append

  let files t ~f =
    let rec loop acc (t : t) =
      let open Ast in
      match t with
      | Element _
      | Special _ -> acc
      | Include fn ->
        String_set.add (f fn) acc
      | Union l ->
        List.fold_left l ~init:acc ~f:loop
      | Diff (l, r) ->
        loop (loop acc l) r
    in
    loop String_set.empty t

  let rec expand (t : t) ~files_contents ~f : (string, Ast.expanded) Ast.t =
    let open Ast in
    match t with
    | Element s -> Element (f (String_with_vars.t s))
    | Special (l, s) -> Special (l, s)
    | Include fn ->
      parse_general (
        String_map.find_exn (f fn) files_contents ~string_of_key:(sprintf "%S")
          ~desc:(fun _ -> "<filename to s-expression>")
      ) ~f:(fun s -> f (String_with_vars.t s))
    | Union l ->
      Union (List.map l ~f:(expand ~files_contents ~f))
    | Diff (l, r) ->
      Diff (expand l ~files_contents ~f, expand r ~files_contents ~f)
end

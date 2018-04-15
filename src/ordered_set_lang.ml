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

type 'ast generic =
  { ast : 'ast
  ; loc : Loc.t option
  }

type ast_expanded = (Loc.t * string, Ast.expanded) Ast.t
type t = ast_expanded generic
let loc t = t.loc

let parse_general sexp ~f =
  let rec of_sexp : Sexp.Ast.t -> _ = function
    | Atom (loc, A "\\") -> Loc.fail loc "unexpected \\"
    | (Atom (_, A "") | Quoted_string (_, _)) as t -> Ast.Element (f t)
    | Atom (loc, A s) as t ->
      if s.[0] = ':' then
        Special (loc, String.sub s ~pos:1 ~len:(String.length s - 1))
      else
        Element (f t)
    | List (_, sexps) -> of_sexps [] sexps
  and of_sexps acc = function
    | Atom (_, A "\\") :: sexps ->
       Diff (Union (List.rev acc), of_sexps [] sexps)
    | elt :: sexps ->
      of_sexps (of_sexp elt :: acc) sexps
    | [] -> Union (List.rev acc)
  in
  of_sexp sexp

let t sexp : t =
  let ast =
    parse_general sexp ~f:(function
      | Atom (loc, A s) | Quoted_string (loc, s) -> (loc, s)
      | List _ -> assert false)
  in
  { ast
  ; loc = Some (Sexp.Ast.loc sexp)
  }

let is_standard t =
  match (t.ast : ast_expanded) with
  | Ast.Special (_, "standard") -> true
  | _ -> false

module type Value = sig
  type t
  type key
  val key : t -> key
end

module type Key = sig
  type t
  val compare : t -> t -> Ordering.t
  module Map : Map.S with type key = t
end

module type S = sig
  type value
  type 'a map

  val eval
    :  t
    -> parse:(loc:Loc.t -> string -> value)
    -> standard:value list
    -> value list

  val eval_unordered
    :  t
    -> parse:(loc:Loc.t -> string -> value)
    -> standard:value map
    -> value map
end

module Make(Key : Key)(Value : Value with type key = Key.t) = struct
  module type Named_values = sig
    type t

    val singleton : Value.t -> t
    val union : t list -> t
    val diff : t -> t -> t
  end

  module Make(M : Named_values) = struct
    let eval t ~parse ~special_values =
      let rec of_ast (t : ast_expanded) =
        let open Ast in
        match t with
        | Element (loc, s) ->
          let x = parse ~loc s in
          M.singleton x
        | Special (loc, name) -> begin
            match String_map.find special_values name with
            | Some x -> x
            | None   -> Loc.fail loc "undefined symbol %s" name
          end
        | Union elts -> M.union (List.map elts ~f:of_ast)
        | Diff (left, right) ->
          let left  = of_ast left  in
          let right = of_ast right in
          M.diff left right
      in
      of_ast t.ast
  end

  module Ordered = Make(struct
      type t = Value.t list

      let singleton x = [x]
      let union = List.flatten
      let diff a b =
        List.filter a ~f:(fun x ->
          List.for_all b ~f:(fun y ->
            Ordering.neq (Key.compare (Value.key x) (Value.key y))))
    end)

  module Unordered = Make(struct
      type t = Value.t Key.Map.t

      let singleton x = Key.Map.singleton (Value.key x) x

      let union l =
        List.fold_left l ~init:Key.Map.empty ~f:(fun acc t ->
          Key.Map.merge acc t ~f:(fun _name x y ->
            match x, y with
            | Some x, _ | _, Some x -> Some x
            | _ -> None))

      let diff a b =
        Key.Map.merge a b ~f:(fun _name x y ->
          match x, y with
          | Some _, None -> x
          | _ -> None)
    end)

  type value = Value.t
  type 'a map = 'a Key.Map.t

  let eval t ~parse ~standard =
    if is_standard t then
      standard (* inline common case *)
    else
      Ordered.eval t ~parse
        ~special_values:(String_map.singleton "standard" standard)

  let eval_unordered t ~parse ~standard =
    if is_standard t then
      standard (* inline common case *)
    else
      Unordered.eval t ~parse
        ~special_values:(String_map.singleton "standard" standard)
end

let standard =
  { ast = Ast.Special (Loc.none, "standard")
  ; loc = None
  }

module Unexpanded = struct
  type ast = (Sexp.Ast.t, Ast.unexpanded) Ast.t
  type t = ast generic
  let t sexp =
    let rec map (t : (Sexp.Ast.t, Ast.expanded) Ast.t) =
      let open Ast in
      match t with
      | Element x -> Element x
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
    { ast = map (parse_general sexp ~f:(fun x -> x))
    ; loc = Some (Sexp.Ast.loc sexp)
    }

  let sexp_of_t t =
    let open Ast in
    let rec loop : ast -> Sexp.t = function
      | Element sexp -> Usexp.Ast.remove_locs sexp
      | Special (_, s) -> Sexp.atom (":" ^ s)
      | Union l -> List (List.map l ~f:loop)
      | Diff (a, b) -> List [loop a; Sexp.unsafe_atom_of_string "\\"; loop b]
      | Include fn -> List [ Sexp.unsafe_atom_of_string ":include"
                           ; String_with_vars.sexp_of_t fn
                           ]
    in
    loop t.ast

  let standard = standard

  let field ?(default=standard) name = Sexp.Of_sexp.field name t ~default

  let files t ~f =
    let rec loop acc (t : ast) =
      let open Ast in
      match t with
      | Element _
      | Special _ -> acc
      | Include fn ->
        String_set.add acc (f fn)
      | Union l ->
        List.fold_left l ~init:acc ~f:loop
      | Diff (l, r) ->
        loop (loop acc l) r
    in
    loop String_set.empty t.ast

  let expand t ~files_contents ~f  =
    let rec expand (t : ast) : ast_expanded =
      let open Ast in
      match t with
      | Element s -> Element (Sexp.Ast.loc s, f (String_with_vars.t s))
      | Special (l, s) -> Special (l, s)
      | Include fn ->
        let sexp =
          let fn = f fn in
          match String_map.find files_contents fn with
          | Some x -> x
          | None ->
            Sexp.code_error
              "Ordered_set_lang.Unexpanded.expand"
              [ "included-file", Quoted_string fn
              ; "files", Sexp.To_sexp.(list string)
                           (String_map.keys files_contents)
              ]
        in
        parse_general sexp ~f:(fun sexp ->
          (Sexp.Ast.loc sexp, f (String_with_vars.t sexp)))
      | Union l -> Union (List.map l ~f:expand)
      | Diff (l, r) ->
        Diff (expand l, expand r)
    in
    { t with ast = expand t.ast }
end

module String = Make(struct
    type t = string
    let compare = String.compare
    module Map = String_map
  end)(struct
    type t = string
    type key = string
    let key x = x
  end)

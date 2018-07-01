open! Import

module Ast = struct
  [@@@warning "-37"]
  type expanded = Expanded
  type unexpanded = Unexpanded
  type ('a, _) t =
    | Element : 'a -> ('a, _) t
    | Standard : ('a, _) t
    | Union : ('a, 'b) t list -> ('a, 'b) t
    | Diff : ('a, 'b) t * ('a, 'b) t -> ('a, 'b) t
    | Include : String_with_vars.t -> ('a, unexpanded) t
end

type 'ast generic =
  { ast : 'ast
  ; loc : Loc.t option
  ; context : Univ_map.t (* Parsing context for Sexp.Of_sexp.parse *)
  }

type ast_expanded = (Loc.t * string, Ast.expanded) Ast.t
type t = ast_expanded generic
let loc t = t.loc

module Parse = struct
  open Stanza.Of_sexp
  open Ast

  let generic ~inc ~elt =
    let open Stanza.Of_sexp in
    let rec one (kind : Stanza.File_kind.t) =
      peek_exn >>= function
      | Atom (loc, A "\\") -> Loc.fail loc "unexpected \\"
      | (Atom (_, A "") | Quoted_string (_, _)) | Template _ ->
        elt >>| fun x -> Element x
      | Atom (loc, A s) -> begin
          match s with
          | ":standard" ->
            junk >>> return Standard
          | ":include" ->
            Loc.fail loc
              "Invalid use of :include, should be: (:include <filename>)"
          | _ when s.[0] = ':' ->
            Loc.fail loc "undefined symbol %s" s
          | _ ->
            elt >>| fun x -> Element x
        end
      | List (_, Atom (loc, A s) :: _) -> begin
          match s, kind with
          | ":include", _ -> inc
          | s, Dune when s <> "" && s.[0] <> '-' && s.[0] <> ':' ->
            Loc.fail loc
              "This atom must be quoted because it is the first element \
               of a list and doesn't start with - or :"
          | _ -> enter (many [] kind)
        end
      | List _ -> enter (many [] kind)
    and many acc kind =
      peek >>= function
      | None -> return (Union (List.rev acc))
      | Some (Atom (_, A "\\")) ->
        junk >>> many [] kind >>| fun to_remove ->
        Diff (Union (List.rev acc), to_remove)
      | Some _ ->
        one kind >>= fun x ->
        many (x :: acc) kind
    in
    Stanza.file_kind () >>= fun kind ->
    match kind with
    | Dune -> many [] kind
    | Jbuild -> one kind

  let with_include ~elt =
    generic ~elt ~inc:(
      sum [ ":include",
            String_with_vars.t >>| fun s ->
            Include s
          ])

  let without_include ~elt =
    generic ~elt ~inc:(
      enter
        (loc >>= fun loc ->
         Loc.fail loc "(:include ...) is not allowed here"))
end


let t =
  let open Stanza.Of_sexp in
  get_all >>= fun context ->
  located (Parse.without_include ~elt:(plain_string (fun ~loc s -> (loc, s))))
  >>| fun (loc, ast) ->
  { ast; loc = Some loc; context }

let is_standard t =
  match (t.ast : ast_expanded) with
  | Ast.Standard -> true
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
    let eval t ~parse ~standard =
      let rec of_ast (t : ast_expanded) =
        let open Ast in
        match t with
        | Element (loc, s) ->
          let x = parse ~loc s in
          M.singleton x
        | Standard -> standard
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
      Ordered.eval t ~parse ~standard

  let eval_unordered t ~parse ~standard =
    if is_standard t then
      standard (* inline common case *)
    else
      Unordered.eval t ~parse ~standard
end

let standard =
  { ast = Ast.Standard
  ; loc = None
  ; context = Univ_map.empty
  }

let field ?(default=standard) name = Sexp.Of_sexp.field name t ~default

module Unexpanded = struct
  type ast = (String_with_vars.t, Ast.unexpanded) Ast.t
  type t = ast generic
  let t =
    let open Stanza.Of_sexp in
    get_all >>= fun context ->
    located (Parse.with_include ~elt:String_with_vars.t)
    >>| fun (loc, ast) ->
    { ast
    ; loc = Some loc
    ; context
    }

  let sexp_of_t t =
    let open Ast in
    let rec loop : ast -> Sexp.t = function
      | Element s -> String_with_vars.sexp_of_t s
      | Standard -> Sexp.atom ":standard"
      | Union l -> List (List.map l ~f:loop)
      | Diff (a, b) -> List [loop a; Sexp.unsafe_atom_of_string "\\"; loop b]
      | Include fn ->
        List [ Sexp.unsafe_atom_of_string ":include"
             ; String_with_vars.sexp_of_t fn
             ]
    in
    loop t.ast

  let standard = standard

  let field ?(default=standard) name = Stanza.Of_sexp.field name t ~default

  let files t ~f =
    let rec loop acc (t : ast) =
      let open Ast in
      match t with
      | Element _ | Standard -> acc
      | Include fn ->
        String.Set.add acc (f fn)
      | Union l ->
        List.fold_left l ~init:acc ~f:loop
      | Diff (l, r) ->
        loop (loop acc l) r
    in
    let syntax =
      match Univ_map.find t.context (Syntax.key Stanza.syntax) with
      | Some (0, _)-> File_tree.Dune_file.Kind.Jbuild
      | None | Some (_, _) -> Dune
    in
    (syntax, loop String.Set.empty t.ast)

  let has_special_forms t =
    let rec loop (t : ast) =
      let open Ast in
      match t with
      | Standard | Include _ -> true
      | Element _ -> false
      | Union l ->
        List.exists l ~f:loop
      | Diff (l, r) ->
        loop l ||
        loop r
    in
    loop t.ast

  type position = Pos | Neg

  let fold_strings t ~init ~f =
    let rec loop (t : ast) pos acc =
      let open Ast in
      match t with
      | Standard | Include _ -> acc
      | Element x -> f pos x acc
      | Union l -> List.fold_left l ~init:acc ~f:(fun acc x -> loop x pos acc)
      | Diff (l, r) ->
        let acc = loop l pos acc in
        let pos =
          match pos with
          | Pos -> Neg
          | Neg -> Pos
        in
        loop r pos acc
    in
    loop t.ast Pos init

  let expand t ~files_contents ~f  =
    let context = t.context in
    let rec expand (t : ast) : ast_expanded =
      let open Ast in
      match t with
      | Element s -> Element (String_with_vars.loc s, f s)
      | Standard -> Standard
      | Include fn ->
        let sexp =
          let fn = f fn in
          match String.Map.find files_contents fn with
          | Some x -> x
          | None ->
            Exn.code_error
              "Ordered_set_lang.Unexpanded.expand"
              [ "included-file", Quoted_string fn
              ; "files", Sexp.To_sexp.(list string)
                           (String.Map.keys files_contents)
              ]
        in
        let open Stanza.Of_sexp in
        parse
          (Parse.without_include
             ~elt:(String_with_vars.t >>| fun s ->
                   (String_with_vars.loc s, f s)))
          context
          sexp
      | Union l -> Union (List.map l ~f:expand)
      | Diff (l, r) ->
        Diff (expand l, expand r)
    in
    { t with ast = expand t.ast }
end

module String = Make(struct
    type t = string
    let compare = String.compare
    module Map = String.Map
  end)(struct
    type t = string
    type key = string
    let key x = x
  end)

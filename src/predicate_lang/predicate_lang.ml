open Stdune
open Dune_sexp

type 'a t =
  | Element of 'a
  | Glob : Dune_glob.V1.t -> string t
  | Compl of 'a t
  | Standard
  | Union of 'a t list
  | Inter of 'a t list

let diff a = function
  | Union [] | Inter [] -> a
  | b -> Inter [ a; Compl b ]

let inter a = Inter a

let compl a = Compl a

let union a = Union a

let not_union a = compl (union a)

let any = Compl (Union [])

let empty = Union []

let rec decode_one f g =
  let open Decoder in
  let bool_ops () =
    sum
      [ ("or", many f g union [])
      ; ("and", many f g inter [])
      ; ("not", many f g not_union [])
      ]
  in
  let elt =
    let+ e = f in
    g e
  in
  peek_exn >>= function
  | Atom (loc, A "\\") -> User_error.raise ~loc [ Pp.text "unexpected \\" ]
  | Atom (_, A "") | Quoted_string (_, _) | Template _ -> elt
  | Atom (loc, A s) -> (
    match s with
    | ":standard" -> junk >>> return Standard
    | ":include" ->
      User_error.raise ~loc
        [ Pp.text ":include isn't supported in the predicate language" ]
    | _ when s.[0] = ':' ->
      User_error.raise ~loc [ Pp.textf "undefined symbol %s" s ]
    | _ -> elt)
  | List (_, Atom (loc, A s) :: _) -> (
    match s with
    | ":include" ->
      User_error.raise ~loc
        [ Pp.text ":include isn't supported in the predicate language" ]
    | "or" | "and" | "not" -> bool_ops ()
    | s when s <> "" && s.[0] <> '-' && s.[0] <> ':' ->
      User_error.raise ~loc
        [ Pp.text
            "This atom must be quoted because it is the first element of a \
             list and doesn't start with - or:"
        ]
    | _ -> enter (many f g union []))
  | List _ -> enter (many f g union [])

and many f g k acc =
  let open Decoder in
  peek >>= function
  | None -> return (k (List.rev acc))
  | Some (Atom (_, A "\\")) ->
    junk >>> many f g union [] >>| fun to_remove ->
    diff (k (List.rev acc)) to_remove
  | Some _ ->
    let* x = decode_one f g in
    many f g k (x :: acc)

and decode f g = many f g union []

let rec encode :
          'a.
             ('a -> Dune_sexp.t)
          -> (Dune_glob.V1.t -> Dune_sexp.t)
          -> 'a t
          -> Dune_sexp.t =
  fun (type a) (f : a -> Dune_sexp.t) glob (t : a t) ->
   let open Encoder in
   match t with
   | Element a -> f a
   | Compl a -> constr "not" (fun x -> encode f glob x) a
   | Standard -> string ":standard"
   | Union xs -> constr "or" (list (encode f glob)) xs
   | Inter xs -> constr "and" (list (encode f glob)) xs
   | Glob g -> glob g

let rec to_dyn : 'a. ('a -> Dyn.t) -> 'a t -> Dyn.t =
  fun (type a) (f : a -> Dyn.t) (t : a t) ->
   let open Dyn in
   match t with
   | Element a -> f a
   | Compl a -> variant "compl" [ to_dyn f a ]
   | Standard -> string ":standard"
   | Union xs -> variant "or" (List.map ~f:(to_dyn f) xs)
   | Inter xs -> variant "and" (List.map ~f:(to_dyn f) xs)
   | Glob glob -> variant "glob" [ Dune_glob.V1.to_dyn glob ]

let rec exec :
          'a. 'a t -> standard:'a t -> equal:('a -> 'a -> bool) -> 'a -> bool =
  fun (type a) (t : a t) ~standard ~equal (elem : a) ->
   match (t : _ t) with
   | Compl t -> not (exec t ~standard ~equal elem)
   | Element f -> equal elem f
   | Union xs -> List.exists ~f:(fun t -> exec t ~standard ~equal elem) xs
   | Inter xs -> List.for_all ~f:(fun t -> exec t ~standard ~equal elem) xs
   | Standard -> exec standard ~standard ~equal elem
   | Glob g -> Dune_glob.V1.test g elem

let rec compare : 'a. ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t =
  fun (type a) (f : a -> a -> Ordering.t) (x : a t) (y : a t) ->
   match (x, y) with
   | Element a, Element b -> f a b
   | Element _, _ -> Lt
   | _, Element _ -> Gt
   | Glob x, Glob y -> Dune_glob.V1.compare x y
   | Glob _, _ -> Lt
   | _, Glob _ -> Gt
   | Compl x, Compl y -> compare f x y
   | Compl _, _ -> Lt
   | _, Compl _ -> Gt
   | Standard, Standard -> Eq
   | Standard, _ -> Lt
   | _, Standard -> Gt
   | Union a, Union b -> List.compare a b ~compare:(compare f)
   | Union _, _ -> Lt
   | _, Union _ -> Gt
   | Inter a, Inter b -> List.compare a b ~compare:(compare f)

module Glob = struct
  module Glob = Dune_glob.V1

  type nonrec t = string t

  let to_dyn t = to_dyn Dyn.string t

  let exec (t : t) ~standard elem = exec t ~standard ~equal:String.equal elem

  let filter (t : t) ~standard elems =
    match t with
    | Inter [] | Union [] -> []
    | _ -> List.filter elems ~f:(fun elem -> exec t ~standard elem)

  let of_glob g = Glob g

  let of_string_list s = Union (List.rev_map s ~f:(fun x -> Element x))

  let of_string_set s = Union (String.Set.to_list_map ~f:(fun x -> Element x) s)

  let compare x y = compare String.compare x y

  let hash t = Poly.hash t

  let encode t =
    encode Dune_sexp.atom_or_quoted_string
      (fun g -> Dune_sexp.atom_or_quoted_string (Dune_glob.V1.to_string g))
      t
end

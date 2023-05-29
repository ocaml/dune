open Stdune
open Dune_sexp

type 'a t =
  | Element of 'a
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

let any = not_union []

let empty = Union []

let rec decode_one f =
  let open Decoder in
  let bool_ops () =
    sum
      [ ("or", many f union [])
      ; ("and", many f inter [])
      ; ("not", many f not_union [])
      ]
  in
  let elt =
    let+ e = f in
    Element e
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
    | _ -> enter (many f union []))
  | List _ -> enter (many f union [])

and many f k acc =
  let open Decoder in
  peek >>= function
  | None -> return (k (List.rev acc))
  | Some (Atom (_, A "\\")) ->
    junk >>> many f union [] >>| fun to_remove ->
    diff (k (List.rev acc)) to_remove
  | Some _ ->
    let* x = decode_one f in
    many f k (x :: acc)

and decode f = many f union []

let rec encode f =
  let open Encoder in
  function
  | Element a -> f a
  | Compl a -> constr "not" (encode f) a
  | Standard -> string ":standard"
  | Union xs -> constr "or" (list (encode f)) xs
  | Inter xs -> constr "and" (list (encode f)) xs

let rec to_dyn f =
  let open Dyn in
  function
  | Element a -> f a
  | Compl a -> variant "compl" [ to_dyn f a ]
  | Standard -> string ":standard"
  | Union xs -> variant "or" (List.map ~f:(to_dyn f) xs)
  | Inter xs -> variant "and" (List.map ~f:(to_dyn f) xs)

let rec test_ t ~standard ~test elem =
  match t with
  | Compl t -> not (test_ t ~standard ~test elem)
  | Element f -> test f elem
  | Union xs -> List.exists ~f:(fun t -> test_ t ~standard ~test elem) xs
  | Inter xs -> List.for_all ~f:(fun t -> test_ t ~standard ~test elem) xs
  | Standard -> test_ standard ~standard ~test elem

let test = test_

let rec compare f x y =
  match (x, y) with
  | Element a, Element b -> f a b
  | Element _, _ -> Lt
  | _, Element _ -> Gt
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

  module Element = struct
    type t =
      | Glob of Glob.t
      | Literal of string

    let to_dyn = function
      | Literal s -> Dyn.variant "Literal" [ Dyn.string s ]
      | Glob g -> Dyn.variant "Glob" [ Glob.to_dyn g ]

    let encode t =
      Dune_sexp.atom_or_quoted_string
      @@
      match t with
      | Literal s -> s
      | Glob g -> Glob.to_string g

    let compare x y =
      match (x, y) with
      | Glob x, Glob y -> Glob.compare x y
      | Glob _, _ -> Lt
      | _, Glob _ -> Gt
      | Literal x, Literal y -> String.compare x y

    let test t s =
      match t with
      | Literal s' -> String.equal s s'
      | Glob g -> Glob.test g s

    let decode =
      let open Dune_sexp.Decoder in
      let+ glob =
        Decoder.plain_string (fun ~loc x -> Glob.of_string_exn loc x)
      in
      Glob glob
  end

  type nonrec t = Element.t t

  let to_dyn t = to_dyn Element.to_dyn t

  let test (t : t) ~standard elem = test t ~standard ~test:Element.test elem

  let of_glob g = Element (Element.Glob g)

  let of_string_list s =
    Union (List.rev_map s ~f:(fun x -> Element (Element.Literal x)))

  let of_string_set s =
    Union (String.Set.to_list_map ~f:(fun x -> Element (Element.Literal x)) s)

  let compare x y = compare Element.compare x y

  let hash t = Poly.hash t

  let decode = decode Element.decode

  let encode t = encode Element.encode t
end

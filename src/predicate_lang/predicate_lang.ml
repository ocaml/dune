open Stdune
open Dune_sexp

type 'a t =
  | True
  | False
  | Element of 'a
  | Not of 'a t
  | Standard
  | Or of 'a t list
  | And of 'a t list

let element a = Element a
let of_list xs = Or (List.rev_map ~f:element xs)
let standard = Standard

let diff a = function
  | Or [] -> a
  | And [] -> Or []
  | b -> And [ a; Not b ]
;;

let not a = Not a
let true_ = True
let false_ = False

let or_ = function
  | [] -> false_
  | [ x ] -> x
  | _ :: _ :: _ as xs -> Or xs
;;

let and_ = function
  | [] -> true_
  | [ x ] -> x
  | _ :: _ :: _ as xs -> And xs
;;

let map t ~f =
  let rec loop = function
    | True -> True
    | False -> False
    | Element a -> Element (f a)
    | Not a -> Not (loop a)
    | Standard -> Standard
    | Or xs -> Or (List.map ~f:loop xs)
    | And xs -> And (List.map ~f:loop xs)
  in
  loop t
;;

let rec decode_one =
  let not_or a = not (Or a) in
  fun f ->
    let open Decoder in
    let bool_ops () =
      sum [ "or", many f or_ []; "and", many f and_ []; "not", many f not_or [] ]
    in
    let elt =
      let+ e = f in
      Element e
    in
    peek_exn
    >>= function
    | Atom (loc, A "\\") -> User_error.raise ~loc [ Pp.text "unexpected \\" ]
    | Atom (_, A "") | Quoted_string (_, _) | Template _ -> elt
    | Atom (loc, A s) ->
      (match s with
       | ":standard" -> junk >>> return Standard
       | ":include" ->
         User_error.raise
           ~loc
           [ Pp.text ":include isn't supported in the predicate language" ]
       | _ when s.[0] = ':' -> User_error.raise ~loc [ Pp.textf "undefined symbol %s" s ]
       | _ -> elt)
    | List (_, Atom (loc, A s) :: _) ->
      (match s with
       | ":include" ->
         User_error.raise
           ~loc
           [ Pp.text ":include isn't supported in the predicate language" ]
       | "or" | "and" | "not" -> bool_ops ()
       | s when s <> "" && s.[0] <> '-' && s.[0] <> ':' ->
         User_error.raise
           ~loc
           [ Pp.text
               "This atom must be quoted because it is the first element of a list and \
                doesn't start with - or:"
           ]
       | _ -> enter (many f or_ []))
    | List _ -> enter (many f or_ [])

and many f k acc =
  let open Decoder in
  peek
  >>= function
  | None -> return (k (List.rev acc))
  | Some (Atom (_, A "\\")) ->
    junk >>> many f or_ [] >>| fun to_remove -> diff (k (List.rev acc)) to_remove
  | Some _ ->
    let* x = decode_one f in
    many f k (x :: acc)

and decode f = many f or_ []

let rec encode f =
  let open Encoder in
  function
  | True -> encode f (Or [])
  | False -> encode f (And [])
  | Element a -> f a
  | Not a -> constr "not" (encode f) a
  | Standard -> string ":standard"
  | Or xs -> constr "or" (list (encode f)) xs
  | And xs -> constr "and" (list (encode f)) xs
;;

let rec to_dyn f =
  let open Dyn in
  function
  | Element a -> f a
  | True -> variant "True" []
  | False -> variant "False" []
  | Not a -> variant "compl" [ to_dyn f a ]
  | Standard -> string ":standard"
  | Or xs -> variant "or" (List.map ~f:(to_dyn f) xs)
  | And xs -> variant "and" (List.map ~f:(to_dyn f) xs)
;;

let rec test_ t ~standard ~test elem =
  match t with
  | True -> true
  | False -> false
  | Not t -> Stdlib.not (test_ t ~standard ~test elem)
  | Element f -> test f elem
  | Or xs -> List.exists ~f:(fun t -> test_ t ~standard ~test elem) xs
  | And xs -> List.for_all ~f:(fun t -> test_ t ~standard ~test elem) xs
  | Standard -> test_ standard ~standard ~test elem
;;

let test = test_

let rec compare f x y =
  match x, y with
  | True, True -> Eq
  | True, _ -> Gt
  | _, True -> Lt
  | Element a, Element b -> f a b
  | Element _, _ -> Lt
  | _, Element _ -> Gt
  | Not x, Not y -> compare f x y
  | Not _, _ -> Lt
  | _, Not _ -> Gt
  | Standard, Standard -> Eq
  | Standard, _ -> Lt
  | _, Standard -> Gt
  | Or a, Or b -> List.compare a b ~compare:(compare f)
  | Or _, _ -> Lt
  | _, Or _ -> Gt
  | And a, And b -> List.compare a b ~compare:(compare f)
  | And _, _ -> Lt
  | _, And _ -> Gt
  | False, False -> Eq
;;

module Glob = struct
  module Glob = Dune_glob.V1

  module Element = struct
    type t =
      | Glob of Glob.t
      | Literal of string

    let to_dyn = function
      | Literal s -> Dyn.variant "Literal" [ Dyn.string s ]
      | Glob g -> Dyn.variant "Glob" [ Glob.to_dyn g ]
    ;;

    (* CR-someday amokhov: The [_exn] suffix is here because [Glob.to_string] can actually
       raise. We should clean this all up, at least use the [_exn] suffix consistently. *)
    let digest_exn = function
      | Literal s -> Dune_digest.generic (0, s)
      | Glob g -> Dune_digest.generic (1, Glob.to_string g)
    ;;

    let encode t =
      Dune_sexp.atom_or_quoted_string
      @@
      match t with
      | Literal s -> s
      | Glob g -> Glob.to_string g
    ;;

    let compare x y =
      match x, y with
      | Glob x, Glob y -> Glob.compare x y
      | Glob _, _ -> Lt
      | _, Glob _ -> Gt
      | Literal x, Literal y -> String.compare x y
    ;;

    let test t s =
      match t with
      | Literal s' -> String.equal s s'
      | Glob g -> Glob.test g s
    ;;

    let decode =
      let open Dune_sexp.Decoder in
      let+ glob = Decoder.plain_string (fun ~loc x -> Glob.of_string_exn loc x) in
      Glob glob
    ;;
  end

  type nonrec t = Element.t t

  let to_dyn t = to_dyn Element.to_dyn t
  let test (t : t) ~standard elem = test t ~standard ~test:Element.test elem
  let of_glob g = Element (Element.Glob g)
  let of_string_list s = Or (List.rev_map s ~f:(fun x -> Element (Element.Literal x)))

  let of_string_set s =
    Or (String.Set.to_list_map ~f:(fun x -> Element (Element.Literal x)) s)
  ;;

  let compare x y = compare Element.compare x y
  let equal x y = Ordering.is_eq (compare x y)
  let hash t = Poly.hash t
  let decode = decode Element.decode
  let encode t = encode Element.encode t
  let digest_exn t = map t ~f:Element.digest_exn |> Dune_digest.generic
end

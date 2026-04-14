type _ t =
  | Unit : unit t
  | Bool : bool t
  | Int : int t
  | String : string t
  | Option : 'a t -> 'a option t
  | List : 'a t -> 'a list t
  | Array : 'a t -> 'a array t
  | Pair : 'a t * 'b t -> ('a * 'b) t
  | Triple : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Fix : 'a t Lazy.t -> 'a t
  | Record : string * 'a field list -> 'a t
  | Variant : string * 'a case list -> 'a t
  | View :
      { repr : 'b t
      ; to_ : 'a -> 'b
      }
      -> 'a t
  | Abstract : { to_dyn : 'a -> Dyn.t } -> 'a t

and 'a field =
  | Field :
      { name : string
      ; repr : 'b t
      ; get : 'a -> 'b
      }
      -> 'a field

and 'a case =
  | Case0 :
      { tag : string
      ; test : 'a -> bool
      }
      -> 'a case
  | Case1 :
      { tag : string
      ; repr : 'b t
      ; proj : 'a -> 'b option
      }
      -> 'a case

type 'a repr = 'a t

module type S = sig
  type t

  val repr : t repr
end

module type S1 = sig
  type 'a t

  val repr : 'a repr -> 'a t repr
end

module type S2 = sig
  type ('a, 'b) t

  val repr : 'a repr -> 'b repr -> ('a, 'b) t repr
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  let repr first second third = Triple (first, second, third)
end

module T4 = struct
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  let repr first second third fourth =
    View
      { repr = Pair (first, Triple (second, third, fourth))
      ; to_ = (fun (first, second, third, fourth) -> first, (second, third, fourth))
      }
  ;;
end

let unit = Unit
let bool = Bool
let int = Int
let string = String
let option repr = Option repr
let list repr = List repr
let array repr = Array repr
let pair left right = Pair (left, right)
let triple first second third = Triple (first, second, third)

let fix f =
  let rec repr = Fix (lazy (f repr)) in
  repr
;;

let view repr ~to_ = View { repr; to_ }
let field name repr ~get = Field { name; repr; get }
let record name fields = Record (name, fields)
let case tag repr ~proj = Case1 { tag; repr; proj }
let case0 tag ~test = Case0 { tag; test }
let variant name cases = Variant (name, cases)
let repr_for_to_dyn to_dyn = Abstract { to_dyn }
let abstract to_dyn = Abstract { to_dyn }

let rec to_dyn : type a. a repr -> a -> Dyn.t =
  fun repr value ->
  match repr with
  | Unit -> Dyn.unit value
  | Bool -> Dyn.bool value
  | Int -> Dyn.int value
  | String -> Dyn.string value
  | Option repr -> Dyn.option (to_dyn repr) value
  | List repr -> Dyn.list (to_dyn repr) value
  | Array repr -> Dyn.array (to_dyn repr) value
  | Pair (left, right) ->
    let left_value, right_value = value in
    Dyn.pair (to_dyn left) (to_dyn right) (left_value, right_value)
  | Triple (first, second, third) ->
    let first_value, second_value, third_value = value in
    Dyn.triple
      (to_dyn first)
      (to_dyn second)
      (to_dyn third)
      (first_value, second_value, third_value)
  | Fix repr -> to_dyn (Lazy.force repr) value
  | Record (_, fields) -> Dyn.record (to_dyn_fields fields value)
  | Variant (type_name, cases) -> to_dyn_case type_name cases value
  | View { repr; to_ } -> to_dyn repr (to_ value)
  | Abstract { to_dyn; _ } -> to_dyn value

and to_dyn_fields : type a. a field list -> a -> (string * Dyn.t) list =
  fun fields value ->
  match fields with
  | [] -> []
  | Field { name; repr; get } :: rest ->
    (name, to_dyn repr (get value)) :: to_dyn_fields rest value

and to_dyn_case : type a. string -> a case list -> a -> Dyn.t =
  fun type_name cases value ->
  match cases with
  | [] ->
    Code_error.raise
      "Repr.variant: value did not match any case"
      [ "type_name", Dyn.string type_name ]
  | Case0 { tag; test } :: rest ->
    if test value then Dyn.variant tag [] else to_dyn_case type_name rest value
  | Case1 { tag; repr; proj } :: rest ->
    (match proj value with
     | Some argument -> Dyn.variant tag [ to_dyn repr argument ]
     | None -> to_dyn_case type_name rest value)
;;

module Make (T : S) = struct
  let to_dyn = to_dyn T.repr
end

module Make1 (T : S1) = struct
  let to_dyn dyn_of = to_dyn (T.repr (repr_for_to_dyn dyn_of))
end

module Make2 (T : S2) = struct
  let to_dyn dyn_of_left dyn_of_right =
    to_dyn (T.repr (repr_for_to_dyn dyn_of_left) (repr_for_to_dyn dyn_of_right))
  ;;
end

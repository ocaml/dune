open Stdune
open Dune_sexp

module Op = struct
  type t =
    | Eq
    | Gt
    | Gte
    | Lte
    | Lt
    | Neq

  let eval t (x : Ordering.t) =
    match t, x with
    | (Eq | Gte | Lte), Eq | (Neq | Lt | Lte), Lt | (Neq | Gt | Gte), Gt -> true
    | _, _ -> false
  ;;

  let compare a b : Ordering.t =
    match a, b with
    | Eq, Eq -> Eq
    | Eq, _ -> Lt
    | _, Eq -> Gt
    | Gt, Gt -> Eq
    | Gt, _ -> Gt
    | _, Gt -> Lt
    | Gte, Gte -> Eq
    | Gte, _ -> Gt
    | _, Gte -> Lt
    | Lte, Lte -> Eq
    | Lte, _ -> Lt
    | _, Lte -> Gt
    | Lt, Lt -> Eq
    | Lt, _ -> Lt
    | _, Lt -> Gt
    | Neq, Neq -> Eq
  ;;

  let to_dyn =
    let open Dyn in
    function
    | Eq -> string "Eq"
    | Gt -> string "Gt"
    | Gte -> string "Gte"
    | Lte -> string "Lte"
    | Lt -> string "Lt"
    | Neq -> string "Neq"
  ;;

  let all = [ "=", Eq; ">=", Gte; "<=", Lte; ">", Gt; "<", Lt; "<>", Neq ]

  let encode x =
    atom
    @@
    match List.assoc (List.map ~f:Tuple.T2.swap all) x with
    | Some x -> x
    | None -> Code_error.raise "Unknown op" []
  ;;
end

type t =
  | Const of bool
  | Not of t
  | Expr of String_with_vars.t
  | And of t list
  | Or of t list
  | Compare of Op.t * String_with_vars.t * String_with_vars.t

let rec map_string_with_vars t ~f =
  match t with
  | Const _ -> t
  | Not t -> Not (map_string_with_vars t ~f)
  | Expr e -> Expr (f e)
  | And t -> And (List.map ~f:(map_string_with_vars ~f) t)
  | Or t -> Or (List.map ~f:(map_string_with_vars ~f) t)
  | Compare (o, s1, s2) -> Compare (o, f s1, f s2)
;;

let true_ = Const true

let rec to_dyn =
  let open Dyn in
  function
  | Const b -> variant "Const" [ bool b ]
  | Not t -> variant "Not" [ to_dyn t ]
  | Expr e -> variant "Expr" [ String_with_vars.to_dyn e ]
  | And t -> variant "And" (List.map ~f:to_dyn t)
  | Or t -> variant "Or" (List.map ~f:to_dyn t)
  | Compare (o, s1, s2) ->
    variant
      "Compare"
      [ Op.to_dyn o; String_with_vars.to_dyn s1; String_with_vars.to_dyn s2 ]
;;

let rec encode =
  let open Encoder in
  function
  | Const b -> bool b
  | Not t -> List [ atom "not"; encode t ]
  | Expr e -> String_with_vars.encode e
  | And t -> List (atom "and" :: List.map ~f:encode t)
  | Or t -> List (atom "or" :: List.map ~f:encode t)
  | Compare (o, s1, s2) ->
    List [ Op.encode o; String_with_vars.encode s1; String_with_vars.encode s2 ]
;;

let decode_gen decode_string =
  let open Decoder in
  let ops =
    List.map Op.all ~f:(fun (name, op) ->
      ( name
      , let+ x = decode_string
        and+ y = decode_string in
        Compare (op, x, y) ))
  in
  let decode =
    fix (fun t ->
      sum
        ~force_parens:true
        (("or", repeat t >>| fun x -> Or x)
         :: ("and", repeat t >>| fun x -> And x)
         :: ("not", Syntax.since Stanza.syntax (3, 2) >>> t >>| fun x -> Not x)
         :: ops)
      <|> let+ v = decode_string in
          Expr v)
  in
  let+ () = Syntax.since Stanza.syntax (1, 1)
  and+ decode = decode in
  decode
;;

let decode = decode_gen String_with_vars.decode
let decode_manually f = decode_gen (String_with_vars.decode_manually f)

let rec compare_no_loc a b : Ordering.t =
  let open Ordering.O in
  match a, b with
  | Const a, Const b -> Bool.compare a b
  | Const _, _ -> Lt
  | _, Const _ -> Gt
  | Not a, Not b -> compare_no_loc a b
  | Not _, _ -> Lt
  | _, Not _ -> Gt
  | Expr a, Expr b -> String_with_vars.compare_no_loc a b
  | Expr _, _ -> Lt
  | _, Expr _ -> Gt
  | And a, And b | Or a, Or b -> List.compare ~compare:compare_no_loc a b
  | And _, _ -> Lt
  | _, And _ -> Gt
  | Compare (o1, s1, s2), Compare (o2, s3, s4) ->
    let= () = Op.compare o1 o2 in
    let= () = String_with_vars.compare_no_loc s1 s3 in
    String_with_vars.compare_no_loc s2 s4
  | Compare _, _ -> Lt
  | _, Compare _ -> Gt
;;

let equal_no_loc a b = Ordering.is_eq (compare_no_loc a b)

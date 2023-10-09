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

  let equal a b =
    match a, b with
    | Eq, Eq -> true
    | Gt, Gt -> true
    | Gte, Gte -> true
    | Lte, Lte -> true
    | Lt, Lt -> true
    | Neq, Neq -> true
    | _ -> false
  ;;

  let by_string = [ "=", Eq; ">=", Gte; "<=", Lte; ">", Gt; "<", Lt; "<>", Neq ]
  let to_string t = fst (List.find_exn by_string ~f:(fun (_, op) -> equal op t))

  let encode t =
    let open Encoder in
    string (to_string t)
  ;;
end

type 'string ast =
  | Const of bool
  | Not of 'string ast
  | Expr of 'string
  | And of 'string ast list
  | Or of 'string ast list
  | Compare of Op.t * 'string * 'string

module Ast = struct
  type 'string t = 'string ast

  let true_ = Const true
  let false_ = Const false

  let rec to_dyn string_to_dyn =
    let open Dyn in
    function
    | Const b -> variant "Const" [ bool b ]
    | Not t -> variant "Not" [ to_dyn string_to_dyn t ]
    | Expr e -> variant "Expr" [ string_to_dyn e ]
    | And t -> variant "And" (List.map ~f:(to_dyn string_to_dyn) t)
    | Or t -> variant "Or" (List.map ~f:(to_dyn string_to_dyn) t)
    | Compare (o, s1, s2) ->
      variant "Compare" [ Op.to_dyn o; string_to_dyn s1; string_to_dyn s2 ]
  ;;

  let decode decode_string =
    let open Decoder in
    let ops =
      List.map Op.by_string ~f:(fun (name, op) ->
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

  let rec encode encode_string t =
    let open Encoder in
    match t with
    | Const true -> string "true"
    | Const false -> string "false"
    | Not t -> List [ string "not"; encode encode_string t ]
    | Expr e -> encode_string e
    | And ts -> List (string "and" :: List.map ts ~f:(encode encode_string))
    | Or ts -> List (string "or" :: List.map ts ~f:(encode encode_string))
    | Compare (o, s1, s2) -> List [ Op.encode o; encode_string s1; encode_string s2 ]
  ;;
end

type t = String_with_vars.t ast

let true_ = Ast.true_
let false_ = Ast.false_
let to_dyn = Ast.to_dyn String_with_vars.to_dyn
let decode = Ast.decode String_with_vars.decode
let encode = Ast.encode String_with_vars.encode

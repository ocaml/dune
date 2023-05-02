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
    match (t, x) with
    | (Eq | Gte | Lte), Eq | (Neq | Lt | Lte), Lt | (Neq | Gt | Gte), Gt -> true
    | _, _ -> false

  let to_dyn =
    let open Dyn in
    function
    | Eq -> string "Eq"
    | Gt -> string "Gt"
    | Gte -> string "Gte"
    | Lte -> string "Lte"
    | Lt -> string "Lt"
    | Neq -> string "Neq"
end

type t =
  | Const of bool
  | Not of t
  | Expr of String_with_vars.t
  | And of t list
  | Or of t list
  | Compare of Op.t * String_with_vars.t * String_with_vars.t

let true_ = Const true

let rec eval t ~dir ~f =
  let open Memo.O in
  match t with
  | Const x -> Memo.return x
  | Expr sw -> (
    String_with_vars.expand sw ~mode:Single ~dir ~f >>| function
    | String "true" -> true
    | String "false" -> false
    | _ ->
      let loc = String_with_vars.loc sw in
      User_error.raise ~loc
        [ Pp.text "This value must be either true or false" ])
  | And xs -> Memo.List.map xs ~f:(eval ~f ~dir) >>| List.for_all ~f:Fun.id
  | Or xs -> Memo.List.map xs ~f:(eval ~f ~dir) >>| List.exists ~f:Fun.id
  | Not t -> eval t ~f ~dir >>| not
  | Compare (op, x, y) ->
    let+ x = String_with_vars.expand x ~mode:Many ~dir ~f
    and+ y = String_with_vars.expand y ~mode:Many ~dir ~f in
    Op.eval op (Value.L.compare_vals ~dir x y)

let rec to_dyn =
  let open Dyn in
  function
  | Const b -> variant "Const" [ bool b ]
  | Not t -> variant "Not" [ to_dyn t ]
  | Expr e -> variant "Expr" [ String_with_vars.to_dyn e ]
  | And t -> variant "And" (List.map ~f:to_dyn t)
  | Or t -> variant "Or" (List.map ~f:to_dyn t)
  | Compare (o, s1, s2) ->
    variant "Compare"
      [ Op.to_dyn o; String_with_vars.to_dyn s1; String_with_vars.to_dyn s2 ]

let ops =
  [ ("=", Op.Eq); (">=", Gte); ("<=", Lte); (">", Gt); ("<", Lt); ("<>", Neq) ]

let decode_gen decode_string =
  let open Decoder in
  let ops =
    List.map ops ~f:(fun (name, op) ->
        ( name
        , let+ x = decode_string
          and+ y = decode_string in
          Compare (op, x, y) ))
  in
  let decode =
    fix (fun t ->
        sum ~force_parens:true
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

let decode = decode_gen String_with_vars.decode

let decode_manually f = decode_gen (String_with_vars.decode_manually f)

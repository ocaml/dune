open Stdune

module Value = struct
  type t =
    | String_literal of string
    | Variable of Package_variable_name.t

  let to_dyn = function
    | String_literal s -> Dyn.variant "String_literal" [ Dyn.string s ]
    | Variable v -> Dyn.variant "Variable" [ Package_variable_name.to_dyn v ]
  ;;

  let compare a b =
    match a, b with
    | String_literal a, String_literal b -> String.compare a b
    | String_literal _, _ -> Lt
    | _, String_literal _ -> Gt
    | Variable a, Variable b -> Package_variable_name.compare a b
  ;;

  let encode = function
    | String_literal s -> Dune_sexp.Encoder.string s
    | Variable v -> Package_variable_name.Project.encode v
  ;;

  let decode =
    let open Dune_sexp.Decoder in
    (let+ variable = Package_variable_name.Project.decode in
     Variable variable)
    <|> let+ s = string in
        String_literal s
  ;;
end

module T = struct
  type t =
    | Bvar of Package_variable_name.t
    | Uop of Relop.t * Value.t
    | Bop of Relop.t * Value.t * Value.t
    | And of t list
    | Or of t list

  let rec to_dyn =
    let open Dyn in
    function
    | Bvar v -> variant "Bvar" [ Package_variable_name.to_dyn v ]
    | Uop (b, x) -> variant "Uop" [ Relop.to_dyn b; Value.to_dyn x ]
    | Bop (b, x, y) -> variant "Bop" [ Relop.to_dyn b; Value.to_dyn x; Value.to_dyn y ]
    | And t -> variant "And" (List.map ~f:to_dyn t)
    | Or t -> variant "Or" (List.map ~f:to_dyn t)
  ;;

  let rec compare a b =
    let open Ordering.O in
    match a, b with
    | Bvar a, Bvar b -> Package_variable_name.compare a b
    | Bvar _, _ -> Lt
    | _, Bvar _ -> Gt
    | Uop (a_op, a_value), Uop (b_op, b_value) ->
      let= () = Relop.compare a_op b_op in
      Value.compare a_value b_value
    | Uop _, _ -> Lt
    | _, Uop _ -> Gt
    | Bop (a_op, a_lhs, a_rhs), Bop (b_op, b_lhs, b_rhs) ->
      let= () = Relop.compare a_op b_op in
      let= () = Value.compare a_lhs b_lhs in
      Value.compare a_rhs b_rhs
    | Bop _, _ -> Lt
    | _, Bop _ -> Gt
    | And a, And b -> List.compare a b ~compare
    | And _, _ -> Lt
    | _, And _ -> Gt
    | Or a, Or b -> List.compare a b ~compare
  ;;
end

include T
include Comparable.Make (T)

let rec encode c =
  let open Dune_sexp.Encoder in
  match c with
  | Bvar x -> Package_variable_name.Project.encode x
  | Uop (op, x) -> pair Relop.encode Value.encode (op, x)
  | Bop (op, x, y) -> triple Relop.encode Value.encode Value.encode (op, x, y)
  | And conjuncts -> list sexp (string "and" :: List.map ~f:encode conjuncts)
  | Or disjuncts -> list sexp (string "or" :: List.map ~f:encode disjuncts)
;;

let logical_op t =
  let open Dune_sexp.Decoder in
  let+ x = repeat t
  and+ version = Dune_sexp.Syntax.get_exn Stanza.syntax
  and+ loc = loc in
  let empty_list_rejected_since = 3, 9 in
  if List.is_empty x && version >= empty_list_rejected_since
  then
    Dune_sexp.Syntax.Error.deleted_in
      loc
      Stanza.syntax
      empty_list_rejected_since
      ~what:"Logical operators with no arguments";
  x
;;

let decode =
  let open Dune_sexp.Decoder in
  let ops =
    List.map Relop.map ~f:(fun (name, op) ->
      ( name
      , let+ x = Value.decode
        and+ y = maybe Value.decode
        and+ loc = loc
        and+ version = Dune_sexp.Syntax.get_exn Stanza.syntax in
        match y with
        | None -> Uop (op, x)
        | Some y ->
          if version < (2, 1)
          then
            Dune_sexp.Syntax.Error.since
              loc
              Stanza.syntax
              (2, 1)
              ~what:(sprintf "Passing two arguments to %s" name);
          Bop (op, x, y) ))
  in
  let ops =
    ( "!="
    , let+ loc = loc in
      User_error.raise ~loc [ Pp.text "Use <> instead of !=" ] )
    :: ops
  in
  fix (fun t ->
    let logops =
      [ ( "and"
        , let+ x = logical_op t in
          And x )
      ; ( "or"
        , let+ x = logical_op t in
          Or x )
      ]
    in
    peek_exn
    >>= function
    | Atom (_loc, A s) when String.is_prefix s ~prefix:":" ->
      let+ () = junk in
      Bvar (Package_variable_name.of_string (String.drop s 1))
    | _ -> sum (ops @ logops))
;;

let equal x y = Ordering.is_eq (compare x y)

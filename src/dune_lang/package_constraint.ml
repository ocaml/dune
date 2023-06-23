open Stdune

module Op = struct
  type t =
    | Eq
    | Gte
    | Lte
    | Gt
    | Lt
    | Neq

  let equal a b =
    match (a, b) with
    | Eq, Eq | Gte, Gte | Lte, Lte | Gt, Gt | Lt, Lt | Neq, Neq -> true
    | _ -> false

  let map =
    [ ("=", Eq); (">=", Gte); ("<=", Lte); (">", Gt); ("<", Lt); ("<>", Neq) ]

  let to_dyn t =
    Dyn.variant (fst (List.find_exn ~f:(fun (_, op) -> equal t op) map)) []

  let encode x =
    let f (_, op) = equal x op in
    (* Assumes the [map] is complete, so exception is impossible *)
    List.find_exn ~f map |> fst |> Dune_sexp.Encoder.string
end

module Var = struct
  type t =
    | Literal of string
    | Var of string

  let encode = function
    | Literal v -> Dune_sexp.Encoder.string v
    | Var v -> Dune_sexp.Encoder.string (":" ^ v)

  let decode =
    let open Dune_sexp.Decoder in
    let+ s = string in
    if String.is_prefix s ~prefix:":" then Var (String.drop s 1) else Literal s

  let to_dyn = function
    | Literal v -> Dyn.String v
    | Var v -> Dyn.String (":" ^ v)
end

type t =
  | Bvar of Var.t
  | Uop of Op.t * Var.t
  | Bop of Op.t * Var.t * Var.t
  | And of t list
  | Or of t list

let rec encode c =
  let open Dune_sexp.Encoder in
  match c with
  | Bvar x -> Var.encode x
  | Uop (op, x) -> pair Op.encode Var.encode (op, x)
  | Bop (op, x, y) -> triple Op.encode Var.encode Var.encode (op, x, y)
  | And conjuncts -> list sexp (string "and" :: List.map ~f:encode conjuncts)
  | Or disjuncts -> list sexp (string "or" :: List.map ~f:encode disjuncts)

let logical_op t =
  let open Dune_sexp.Decoder in
  let+ x = repeat t
  and+ version = Dune_sexp.Syntax.get_exn Stanza.syntax
  and+ loc = loc in
  let empty_list_rejected_since = (3, 9) in
  if List.is_empty x && version >= empty_list_rejected_since then
    Dune_sexp.Syntax.Error.deleted_in loc Stanza.syntax
      empty_list_rejected_since ~what:"Logical operators with no arguments";
  x

let decode =
  let open Dune_sexp.Decoder in
  let ops =
    List.map Op.map ~f:(fun (name, op) ->
        ( name
        , let+ x = Var.decode
          and+ y = maybe Var.decode
          and+ loc = loc
          and+ version = Dune_sexp.Syntax.get_exn Stanza.syntax in
          match y with
          | None -> Uop (op, x)
          | Some y ->
            if version < (2, 1) then
              Dune_sexp.Syntax.Error.since loc Stanza.syntax (2, 1)
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
      peek_exn >>= function
      | Atom (_loc, A s) when String.is_prefix s ~prefix:":" ->
        let+ () = junk in
        Bvar (Var (String.drop s 1))
      | _ -> sum (ops @ logops))

let rec to_dyn =
  let open Dyn in
  function
  | Bvar v -> variant "Bvar" [ Var.to_dyn v ]
  | Uop (b, x) -> variant "Uop" [ Op.to_dyn b; Var.to_dyn x ]
  | Bop (b, x, y) -> variant "Bop" [ Op.to_dyn b; Var.to_dyn x; Var.to_dyn y ]
  | And t -> variant "And" (List.map ~f:to_dyn t)
  | Or t -> variant "Or" (List.map ~f:to_dyn t)

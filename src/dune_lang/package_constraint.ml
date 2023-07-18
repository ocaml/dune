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

module Variable = struct
  type t = { name : string }

  let of_name name = { name }

  let to_dyn { name } = Dyn.record [ ("name", Dyn.string name) ]

  let encode { name } = Dune_sexp.Encoder.string (":" ^ name)

  let decode =
    Dune_sexp.Decoder.atom_matching ~desc:"variable" (fun s ->
        if String.is_prefix s ~prefix:":" then Some (of_name (String.drop s 1))
        else None)
end

module Value = struct
  type t =
    | String_literal of string
    | Variable of Variable.t

  let to_dyn = function
    | String_literal s -> Dyn.variant "String_literal" [ Dyn.string s ]
    | Variable v -> Dyn.variant "Variable" [ Variable.to_dyn v ]

  let encode = function
    | String_literal s -> Dune_sexp.Encoder.string s
    | Variable v -> Variable.encode v

  let decode =
    let open Dune_sexp.Decoder in
    (let+ variable = Variable.decode in
     Variable variable)
    <|> let+ s = string in
        String_literal s
end

type t =
  | Bvar of Variable.t
  | Uop of Op.t * Value.t
  | Bop of Op.t * Value.t * Value.t
  | And of t list
  | Or of t list

let rec encode c =
  let open Dune_sexp.Encoder in
  match c with
  | Bvar x -> Variable.encode x
  | Uop (op, x) -> pair Op.encode Value.encode (op, x)
  | Bop (op, x, y) -> triple Op.encode Value.encode Value.encode (op, x, y)
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
        , let+ x = Value.decode
          and+ y = maybe Value.decode
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
        Bvar (Variable.of_name (String.drop s 1))
      | _ -> sum (ops @ logops))

let rec to_dyn =
  let open Dyn in
  function
  | Bvar v -> variant "Bvar" [ Variable.to_dyn v ]
  | Uop (b, x) -> variant "Uop" [ Op.to_dyn b; Value.to_dyn x ]
  | Bop (b, x, y) ->
    variant "Bop" [ Op.to_dyn b; Value.to_dyn x; Value.to_dyn y ]
  | And t -> variant "And" (List.map ~f:to_dyn t)
  | Or t -> variant "Or" (List.map ~f:to_dyn t)

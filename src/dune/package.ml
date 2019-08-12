open! Stdune

module Name = struct
  module T =
    Interned.Make
      (struct
        let initial_size = 16

        let resize_policy = Interned.Conservative

        let order = Interned.Natural
      end)
      ()

  include T

  let of_string = make

  let opam_fn (t : t) = to_string t ^ ".opam"

  let meta_fn (t : t) = "META." ^ to_string t

  let version_fn (t : t) = to_string t ^ ".version"

  let pp fmt t = Format.pp_print_string fmt (to_string t)

  let decode = Dune_lang.Decoder.(map string ~f:of_string)

  let encode t = Dune_lang.Encoder.(string (to_string t))

  let to_dyn t = Dyn.Encoder.string (to_string t)

  module Infix = Comparator.Operators (T)
end

module Dependency = struct
  module Op = struct
    type t =
      | Eq
      | Gte
      | Lte
      | Gt
      | Lt
      | Neq

    let map =
      [ ("=", Eq); (">=", Gte); ("<=", Lte); (">", Gt); ("<", Lt); ("<>", Neq) ]

    let to_dyn =
      let open Dyn.Encoder in
      function
      | Eq ->
          string "Eq"
      | Gt ->
          string "Gt"
      | Gte ->
          string "Gte"
      | Lte ->
          string "Lte"
      | Lt ->
          string "Lt"
      | Neq ->
          string "Neq"

    let to_relop : t -> OpamParserTypes.relop = function
      | Eq ->
          `Eq
      | Gte ->
          `Geq
      | Lte ->
          `Leq
      | Gt ->
          `Gt
      | Lt ->
          `Lt
      | Neq ->
          `Neq
  end

  module Constraint = struct
    module Var = struct
      type t =
        | QVar of string
        | Var of string

      let decode =
        let open Dune_lang.Decoder in
        let+ s = string in
        if String.is_prefix s ~prefix:":" then
          Var (String.drop s 1)
        else
          QVar s

      let to_opam : t -> OpamParserTypes.value =
        let nopos = Opam_file.nopos in
        function QVar x -> String (nopos, x) | Var x -> Ident (nopos, x)
    end

    type t =
      | Bvar of Var.t
      | Uop of Op.t * Var.t
      | And of t list
      | Or of t list

    let decode =
      let open Dune_lang.Decoder in
      let ops =
        List.map Op.map ~f:(fun (name, op) ->
            ( name
            , let+ x = Var.decode in
              Uop (op, x) ))
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
              , let+ x = repeat t in
                And x )
            ; ( "or"
              , let+ x = repeat t in
                Or x )
            ]
          in
          peek_exn
          >>= function
          | Atom (_loc, A s) when String.is_prefix s ~prefix:":" ->
              let+ () = junk in
              Bvar (Var (String.drop s 1))
          | _ ->
              sum (ops @ logops))

    let rec to_dyn =
      let open Dyn.Encoder in
      function
      | Bvar (QVar v) ->
          constr "Bvar" [ Dyn.String v ]
      | Bvar (Var v) ->
          constr "Bvar" [ Dyn.String (":" ^ v) ]
      | Uop (b, QVar v) ->
          constr "Uop" [ Op.to_dyn b; Dyn.String v ]
      | Uop (b, Var v) ->
          constr "Uop" [ Op.to_dyn b; Dyn.String (":" ^ v) ]
      | And t ->
          constr "And" (List.map ~f:to_dyn t)
      | Or t ->
          constr "Or" (List.map ~f:to_dyn t)
  end

  type t =
    { name : Name.t
    ; constraint_ : Constraint.t option
    }

  let decode =
    let open Dune_lang.Decoder in
    let constrained =
      let+ name = Name.decode
      and+ expr = Constraint.decode in
      { name; constraint_ = Some expr }
    in
    if_list ~then_:(enter constrained)
      ~else_:
        (let+ name = Name.decode in
         { name; constraint_ = None })

  let rec opam_constraint : Constraint.t -> OpamParserTypes.value =
    let nopos = Opam_file.nopos in
    function
    | Bvar v ->
        Constraint.Var.to_opam v
    | Uop (op, v) ->
        Prefix_relop (nopos, Op.to_relop op, Constraint.Var.to_opam v)
    | And [ c ] ->
        opam_constraint c
    | And (c :: cs) ->
        Logop (nopos, `And, opam_constraint c, opam_constraint (And cs))
    | Or [ c ] ->
        opam_constraint c
    | Or (c :: cs) ->
        Logop (nopos, `Or, opam_constraint c, opam_constraint (And cs))
    | And [] | Or [] ->
        Code_error.raise "opam_constraint" []

  let opam_depend : t -> OpamParserTypes.value =
    let nopos = Opam_file.nopos in
    fun { name; constraint_ } ->
      let constraint_ = Option.map ~f:opam_constraint constraint_ in
      let pkg : OpamParserTypes.value = String (nopos, Name.to_string name) in
      match constraint_ with
      | None ->
          pkg
      | Some c ->
          Option (nopos, pkg, [ c ])

  let to_dyn { name; constraint_ } =
    let open Dyn.Encoder in
    record
      [ ("name", Name.to_dyn name)
      ; ("constr", Dyn.Option (Option.map ~f:Constraint.to_dyn constraint_))
      ]
end

module Kind = struct
  type has_opam = bool

  type t =
    | Dune of has_opam
    | Opam

  let to_dyn =
    let open Dyn.Encoder in
    function Dune b -> constr "Dune" [ bool b ] | Opam -> constr "Opam" []
end

type t =
  { name : Name.t
  ; loc : Loc.t
  ; synopsis : string option
  ; description : string option
  ; depends : Dependency.t list
  ; conflicts : Dependency.t list
  ; depopts : Dependency.t list
  ; path : Path.Source.t
  ; version : string option
  ; kind : Kind.t
  ; tags : string list
  }

(* Package name are globally unique, so we can reasonably expect that there
   will always be only a single value of type [t] with a given name in memory.
   That's why we only hash the name. *)
let hash t = Name.hash t.name

let decode ~dir =
  let open Dune_lang.Decoder in
  fields
  @@ let+ loc = loc
     and+ name = field "name" Name.decode
     and+ synopsis = field_o "synopsis" string
     and+ description = field_o "description" string
     and+ depends = field ~default:[] "depends" (repeat Dependency.decode)
     and+ conflicts = field ~default:[] "conflicts" (repeat Dependency.decode)
     and+ depopts = field ~default:[] "depopts" (repeat Dependency.decode)
     and+ tags = field "tags" (enter (repeat string)) ~default:[] in
     { name
     ; loc
     ; synopsis
     ; description
     ; depends
     ; conflicts
     ; depopts
     ; path = dir
     ; version = None
     ; kind = Dune false
     ; tags
     }

let to_dyn
    { name
    ; path
    ; version
    ; synopsis
    ; description
    ; depends
    ; conflicts
    ; depopts
    ; kind
    ; tags
    ; loc = _
    } =
  let open Dyn.Encoder in
  record
    [ ("name", Name.to_dyn name)
    ; ("path", Path.Source.to_dyn path)
    ; ("synopsis", option string synopsis)
    ; ("description", option string description)
    ; ("depends", list Dependency.to_dyn depends)
    ; ("conflicts", list Dependency.to_dyn conflicts)
    ; ("depopts", list Dependency.to_dyn depopts)
    ; ("kind", Kind.to_dyn kind)
    ; ("tags", list string tags)
    ; ("version", option string version)
    ]

let opam_file t = Path.Source.relative t.path (Name.opam_fn t.name)

let meta_file t = Path.Source.relative t.path (Name.meta_fn t.name)

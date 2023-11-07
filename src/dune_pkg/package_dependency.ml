open! Stdune
include Dune_lang.Package_dependency

let nopos pelem = { OpamParserTypes.FullPos.pelem; pos = Opam_file.nopos }

module Constraint = struct
  include Dune_lang.Package_constraint

  module Op = struct
    include Op

    let to_relop = function
      | Eq -> nopos `Eq
      | Gte -> nopos `Geq
      | Lte -> nopos `Leq
      | Gt -> nopos `Gt
      | Lt -> nopos `Lt
      | Neq -> nopos `Neq
    ;;

    let to_relop_pelem op =
      let ({ pelem; _ } : OpamParserTypes.FullPos.relop) = to_relop op in
      pelem
    ;;
  end

  module Variable = struct
    include Variable

    let to_opam { name } = nopos (OpamParserTypes.FullPos.Ident name)
    let to_opam_filter { name } = OpamTypes.FIdent ([], OpamVariable.of_string name, None)
  end

  module Value = struct
    include Value

    let to_opam v =
      match v with
      | String_literal x -> nopos (OpamParserTypes.FullPos.String x)
      | Variable variable -> Variable.to_opam variable
    ;;

    let to_opam_filter = function
      | String_literal literal -> OpamTypes.FString literal
      | Variable variable -> Variable.to_opam_filter variable
    ;;
  end

  let rec to_opam_condition = function
    | Bvar variable ->
      OpamTypes.Atom (OpamTypes.Filter (Variable.to_opam_filter variable))
    | Uop (op, value) ->
      OpamTypes.Atom
        (OpamTypes.Constraint (Op.to_relop_pelem op, Value.to_opam_filter value))
    | Bop (op, lhs, rhs) ->
      OpamTypes.Atom
        (OpamTypes.Filter
           (OpamTypes.FOp
              (Value.to_opam_filter lhs, Op.to_relop_pelem op, Value.to_opam_filter rhs)))
    | And conjunction -> OpamFormula.ands (List.map conjunction ~f:to_opam_condition)
    | Or disjunction -> OpamFormula.ors (List.map disjunction ~f:to_opam_condition)
  ;;
end

type context =
  | Root
  | Ctx_and
  | Ctx_or

(* The printer in opam-file-format does not insert parentheses on its own,
   but it is possible to use the [Group] constructor with a singleton to
   force insertion of parentheses. *)
let group e = nopos (Group (nopos [ e ]) : OpamParserTypes.FullPos.value_kind)
let group_if b e = if b then group e else e

let op_list op = function
  | [] ->
    User_error.raise [ Pp.textf "logical operations with no arguments are not supported" ]
  | v :: vs ->
    List.fold_left ~init:v vs ~f:(fun a b ->
      nopos (OpamParserTypes.FullPos.Logop (nopos op, a, b)))
;;

let opam_constraint t : OpamParserTypes.FullPos.value =
  let open OpamParserTypes.FullPos in
  let rec opam_constraint context = function
    | Constraint.Bvar v -> Constraint.Variable.to_opam v
    | Uop (op, x) ->
      nopos (Prefix_relop (Constraint.Op.to_relop op, Constraint.Value.to_opam x))
    | Bop (op, x, y) ->
      nopos
        (Relop
           ( Constraint.Op.to_relop op
           , Constraint.Value.to_opam x
           , Constraint.Value.to_opam y ))
    | And cs -> logical_op `And cs ~inner_ctx:Ctx_and ~group_needed:false
    | Or cs ->
      let group_needed =
        match context with
        | Root -> false
        | Ctx_and -> true
        | Ctx_or -> false
      in
      logical_op `Or cs ~inner_ctx:Ctx_or ~group_needed
  and logical_op op cs ~inner_ctx ~group_needed =
    List.map cs ~f:(opam_constraint inner_ctx) |> op_list op |> group_if group_needed
  in
  opam_constraint Root t
;;

let opam_depend { name; constraint_ } =
  let constraint_ = Option.map ~f:opam_constraint constraint_ in
  let pkg = nopos (OpamParserTypes.FullPos.String (Package_name.to_string name)) in
  match constraint_ with
  | None -> pkg
  | Some c -> nopos (OpamParserTypes.FullPos.Option (pkg, nopos [ c ]))
;;

let list_to_opam_filtered_formula ts =
  List.map ts ~f:(fun { name; constraint_ } ->
    let opam_package_name = Package_name.to_opam_package_name name in
    let condition =
      match constraint_ with
      | None -> OpamTypes.Empty
      | Some constraint_ -> Constraint.to_opam_condition constraint_
    in
    OpamFormula.Atom (opam_package_name, condition))
  |> OpamFormula.ands
;;

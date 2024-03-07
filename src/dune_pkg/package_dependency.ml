open Import
include Dune_lang.Package_dependency

let nopos pelem = { OpamParserTypes.FullPos.pelem; pos = Opam_file.nopos }

module Convert_from_opam_error = struct
  type t =
    | Can't_convert_opam_filter_to_value of OpamTypes.filter
    | Can't_convert_opam_filter_to_condition of OpamTypes.filter
    | Filtered_formula_is_not_a_conjunction_of_atoms of
        { non_atom : OpamTypes.filtered_formula }
end

module Constraint = struct
  include Dune_lang.Package_constraint

  module Op = struct
    include Dune_lang.Relop

    let to_opam : t -> OpamParserTypes.relop = function
      | Eq -> `Eq
      | Gte -> `Geq
      | Lte -> `Leq
      | Gt -> `Gt
      | Lt -> `Lt
      | Neq -> `Neq
    ;;

    let of_opam = function
      | `Eq -> Eq
      | `Geq -> Gte
      | `Leq -> Lte
      | `Gt -> Gt
      | `Lt -> Lt
      | `Neq -> Neq
    ;;

    let to_relop_pelem op =
      let ({ pelem; _ } : OpamParserTypes.FullPos.relop) = nopos (to_opam op) in
      pelem
    ;;
  end

  module Variable = struct
    include Package_variable_name

    let to_opam_filter (t : t) = OpamTypes.FIdent ([], to_opam t, None)
    let to_opam t = nopos (OpamParserTypes.FullPos.Ident (to_string t))
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

    let of_opam_filter (filter : OpamTypes.filter) =
      match filter with
      | FString string -> Ok (Value.String_literal string)
      | FBool true -> Ok (Value.String_literal "true")
      | FBool false -> Ok (Value.String_literal "false")
      | FIdent ([], name, None) -> Ok (Value.Variable (Variable.of_opam name))
      | _ -> Error (Convert_from_opam_error.Can't_convert_opam_filter_to_value filter)
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

  let rec of_opam_filter (filter : OpamTypes.filter) =
    let open Result.O in
    match filter with
    | FIdent ([], name, None) -> Ok (Bvar (Variable.of_opam name))
    | FOp (lhs, relop, rhs) ->
      let op = Op.of_opam relop in
      let+ lhs = Value.of_opam_filter lhs
      and+ rhs = Value.of_opam_filter rhs in
      Bop (op, lhs, rhs)
    | FAnd (lhs, rhs) ->
      let+ lhs = of_opam_filter lhs
      and+ rhs = of_opam_filter rhs in
      And [ lhs; rhs ]
    | FOr (lhs, rhs) ->
      let+ lhs = of_opam_filter lhs
      and+ rhs = of_opam_filter rhs in
      Or [ lhs; rhs ]
    | _ -> Error (Convert_from_opam_error.Can't_convert_opam_filter_to_condition filter)
  ;;

  let rec opt_of_opam_condition (condition : OpamTypes.condition) =
    let open Result.O in
    match condition with
    | Empty -> Ok None
    | Atom (Filter filter) -> of_opam_filter filter >>| Option.some
    | Atom (Constraint (relop, filter)) ->
      let op = Op.of_opam relop in
      let+ value = Value.of_opam_filter filter in
      Some (Uop (op, value))
    | Block formula -> opt_of_opam_condition formula
    | And (lhs, rhs) ->
      let+ lhs = opt_of_opam_condition lhs
      and+ rhs = opt_of_opam_condition rhs in
      (match lhs, rhs with
       | None, None -> None
       | Some x, None | None, Some x -> Some x
       | Some lhs, Some rhs -> Some (And [ lhs; rhs ]))
    | Or (lhs, rhs) ->
      let+ lhs = opt_of_opam_condition lhs
      and+ rhs = opt_of_opam_condition rhs in
      (match lhs, rhs with
       | None, None -> None
       | Some x, None | None, Some x -> Some x
       | Some lhs, Some rhs -> Some (Or [ lhs; rhs ]))
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
      nopos (Prefix_relop (nopos @@ Constraint.Op.to_opam op, Constraint.Value.to_opam x))
    | Bop (op, x, y) ->
      nopos
        (Relop
           ( nopos @@ Constraint.Op.to_opam op
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

let list_of_opam_filtered_formula loc filtered_formula =
  let exception E of Convert_from_opam_error.t in
  try
    OpamFormula.ands_to_list filtered_formula
    |> List.map ~f:(fun (filtered_formula : OpamTypes.filtered_formula) ->
      match filtered_formula with
      | Atom (name, condition) ->
        let name = Package_name.of_opam_package_name name in
        (match Constraint.opt_of_opam_condition condition with
         | Ok constraint_ -> { name; constraint_ }
         | Error error -> raise (E error))
      | non_atom ->
        raise (E (Filtered_formula_is_not_a_conjunction_of_atoms { non_atom })))
  with
  | E e ->
    let message =
      match e with
      | Can't_convert_opam_filter_to_value filter ->
        let filter_string = OpamFilter.to_string filter in
        sprintf
          "Can't convert opam filter '%s' into dune value. Only literal values and \
           global variables may appear in this position."
          filter_string
      | Can't_convert_opam_filter_to_condition filter ->
        let filter_string = OpamFilter.to_string filter in
        sprintf
          "Can't convert opam filter '%s' into dune condition. Only global variables may \
           appear in this position."
          filter_string
      | Filtered_formula_is_not_a_conjunction_of_atoms { non_atom } ->
        let formula_string = OpamFilter.string_of_filtered_formula non_atom in
        sprintf
          "Expected formula to be a conjunction of atoms but encountered non-atom term \
           '%s'"
          formula_string
    in
    User_error.raise ~loc [ Pp.text message ]
;;

open! Import

let rec formula atom_to_dyn (formula_ : _ OpamFormula.formula) =
  match formula_ with
  | Empty -> Dyn.variant "Empty" []
  | Atom x -> Dyn.variant "Atom" [ atom_to_dyn x ]
  | Block formula_ -> Dyn.variant "Block" [ formula atom_to_dyn formula_ ]
  | And (lhs, rhs) -> Dyn.variant "And" [ Dyn.list (formula atom_to_dyn) [ lhs; rhs ] ]
  | Or (lhs, rhs) -> Dyn.variant "Or" [ Dyn.list (formula atom_to_dyn) [ lhs; rhs ] ]
;;

let relop = function
  | `Eq -> Dyn.variant "Eq" []
  | `Neq -> Dyn.variant "Neq" []
  | `Geq -> Dyn.variant "Geq" []
  | `Gt -> Dyn.variant "Gt" []
  | `Leq -> Dyn.variant "Leq" []
  | `Lt -> Dyn.variant "Lt" []
;;

let variable variable_ = Dyn.string (OpamVariable.to_string variable_)
let package_name package_name_ = Dyn.string (OpamPackage.Name.to_string package_name_)

let rec filter (filter_ : OpamTypes.filter) =
  match filter_ with
  | FBool bool -> Dyn.variant "FBool" [ Dyn.bool bool ]
  | FString string -> Dyn.variant "FString" [ Dyn.string string ]
  | FIdent ident ->
    Dyn.variant
      "FIdent"
      [ Dyn.triple
          (Dyn.list (Dyn.option package_name))
          variable
          (Dyn.option (Dyn.pair Dyn.string Dyn.string))
          ident
      ]
  | FOp (lhs, op, rhs) -> Dyn.variant "FOp" [ filter lhs; relop op; filter rhs ]
  | FAnd (lhs, rhs) -> Dyn.variant "FAnd" [ Dyn.list filter [ lhs; rhs ] ]
  | FOr (lhs, rhs) -> Dyn.variant "FOr" [ Dyn.list filter [ lhs; rhs ] ]
  | FNot filter_ -> Dyn.variant "FNot" [ filter filter_ ]
  | FDefined filter_ -> Dyn.variant "FDefined" [ filter filter_ ]
  | FUndef filter_ -> Dyn.variant "FUndef" [ filter filter_ ]
;;

let filter_or_constraint
  atom_to_dyn
  (filter_or_constraint_ : _ OpamTypes.filter_or_constraint)
  =
  match filter_or_constraint_ with
  | Filter filter_ -> Dyn.variant "Filter" [ filter filter_ ]
  | Constraint (op, atom) -> Dyn.variant "Constraint" [ relop op; atom_to_dyn atom ]
;;

let condition = formula (filter_or_constraint filter)
let filtered_formula = formula (Dyn.pair package_name condition)

let package_version package_version_ =
  Dyn.string (OpamPackage.Version.to_string package_version_)
;;

let package (package_ : OpamPackage.t) =
  Dyn.record
    [ "name", package_name package_.name; "version", package_version package_.version ]
;;

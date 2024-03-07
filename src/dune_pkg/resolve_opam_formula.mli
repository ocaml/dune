open! Import

(** Evaluate the filters in a filtered formula returning the resulting formula. *)
val apply_filter
  :  OpamFilter.env
  -> with_test:bool
  -> OpamTypes.filtered_formula
  -> OpamTypes.formula

module Version_constraint : sig
  type t = Dune_lang.Relop.t * Package_version.t
end

module Unsatisfied_formula_hint : sig
  type t =
    | Missing_package of Package_name.t
    | Unsatisfied_version_constraint of
        { package_name : Package_name.t
        ; found_version : Package_version.t
        ; version_constraint : Version_constraint.t
        }

  val to_dyn : t -> Dyn.t
  val pp : t -> 'a Pp.t
end

(** An unsatisfied formula is accompanied by a list of hints rather than a
    single hint as a formula may be unsatisfied for several reasons in the case
    of a formula containing a disjunction. In such a case, fixing one of the
    problems may cause the formula to become satisfiable. *)
type unsatisfied_formula =
  [ `Formula_could_not_be_satisfied of Unsatisfied_formula_hint.t list ]

(** Given a map associating package names with the version of that package in a
    solution, this function evaluates an [OpamFormula.t] to a concrete list of
    package names all contained in the solution with versions which satisfy the
    formula, or an error if the the version constraints or packages in the
    formula can't be satisfied with the packages in the solution. If the
    formula contains disjunctions (the `|` operator in opam) then there may be
    multiple sets of packages that satisfy the formula. In this case the choice
    of which set to return is made arbitrarily. *)
val formula_to_package_names
  :  Package_version.t Package_name.Map.t
  -> OpamTypes.formula
  -> (Package_name.t list, unsatisfied_formula) result

(** Like [formula_to_package_name] but takes an [OpamTypes.filtered_formula]
    and evaluates its filters to produce a formula which is then resolved to a
    list of package names. *)
val filtered_formula_to_package_names
  :  OpamFilter.env
  -> with_test:bool
  -> Package_version.t Package_name.Map.t
  -> OpamTypes.filtered_formula
  -> (Package_name.t list, unsatisfied_formula) result

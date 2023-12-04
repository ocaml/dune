(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Formulas on packages, opt. with sub-formulas on versions, and conversion
    functions *)

(** binary operations (compatible with the Dose type for Cudf operators!) *)
type relop = OpamParserTypes.FullPos.relop_kind (* = [ `Eq | `Neq | `Geq | `Gt | `Leq | `Lt ] *)

(** Version constraints for OPAM *)
type version_constraint = relop * OpamPackage.Version.t

(** Formula atoms for OPAM *)
type atom = OpamPackage.Name.t * version_constraint option

(** Pretty-printing of atoms *)
val string_of_atom: atom -> string

(** The compact atom format used in requests, "pkgOPvers", with '.' allowed
    instead of '=' *)
val short_string_of_atom: atom -> string

(** Parses a package or atom, in a format similar to [short_string_of_atom].
    @raise [Failure] if the format is incorrect *)
val atom_of_string: string -> atom

(** Prints atoms as a conjunction ("&") using the short format *)
val string_of_atoms: atom list -> string

(** Checks if a package verifies an atom *)
val check: atom -> OpamPackage.t -> bool

(** Return all packages satisfying the given atoms from a set (i.e. name
    matching at least one of the atoms, version matching all atoms with the
    appropriate name). If [disj] is true, returns packages that satisfy at
    least one of the constraint of a given name, otherwise that satisfy all
    constraints. *)
val packages_of_atoms:
  ?disj:bool -> OpamPackage.Set.t -> atom list -> OpamPackage.Set.t

(** AND formulas *)
type 'a conjunction = 'a list

(** Pretty print AND formulas *)
val string_of_conjunction: ('a -> string) -> 'a conjunction -> string

(** OR formulas *)
type 'a disjunction = 'a list

(** Pretty print OR formulas *)
val string_of_disjunction: ('a -> string) -> 'a disjunction -> string

(** CNF formulas (Conjunctive Normal Form) *)
type 'a cnf = 'a disjunction conjunction

(** DNF formulas (Disjunctive Normal Form) *)
type 'a dnf = 'a conjunction disjunction

(** Pretty print CNF formulas *)
val string_of_cnf: ('a -> string) -> 'a cnf -> string

(** Pretty print DNF formulas *)
val string_of_dnf: ('a -> string) -> 'a dnf -> string

(** General formulas *)
type 'a formula =
  | Empty
  | Atom of 'a
  | Block of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula

val compare_formula: ('a -> 'a -> int) -> 'a formula -> 'a formula -> int

(** Eval a formula *)
val eval: ('a -> bool) -> 'a formula -> bool

val partial_eval:
  ('a -> [ `Formula of 'b formula | `True | `False ]) ->
  'a formula ->
  [ `Formula of 'b formula | `True | `False ]

(** Check a relational operator against an integer from compare *)
val check_relop: relop -> int -> bool

(** Evaluate a relational operator between versions *)
val eval_relop: relop -> OpamPackage.Version.t -> OpamPackage.Version.t -> bool

val neg_relop: relop -> relop

(** Pretty print a formula *)
val string_of_formula: ('a -> string) -> 'a formula -> string

(** Convert a list of formulas to an AND-formula ([Empty] formulas are
    ignored) *)
val ands: 'a formula list -> 'a formula

(** Converts back an AND-formula to a list (flattens top-level ands) *)
val ands_to_list: 'a formula -> 'a formula list

(** Convert a list of formulas to an OR-formula ([Empty] formulas are
    ignored) *)
val ors: 'a formula list -> 'a formula

(** Converts back an OR-formula to a list (flattens top-level ors) *)
val ors_to_list: 'a formula -> 'a formula list

(** Map on atoms. Atoms for which the given function returns Empty
    will be simply removed *)
val map: ('a -> 'b formula) -> 'a formula -> 'b formula

(** Maps top-down on a formula *)
val map_formula: ('a formula -> 'a formula) -> 'a formula -> 'a formula

(** Maps bottom-up on a formula (atoms first) *)
val map_up_formula: ('a formula -> 'a formula) -> 'a formula -> 'a formula

(** Negates a formula (given the function to negate atoms) *)
val neg: ('a -> 'a) -> 'a formula -> 'a formula

(** Iter function *)
val iter: ('a -> unit) -> 'a formula -> unit

(** Fold function (bottom-up, left-to-right) *)
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b formula -> 'a

(** Fold function (bottom-up, right-to-left) *)
val fold_right: ('a -> 'b -> 'a) -> 'a -> 'b formula -> 'a

(** Sort formula, using [compare] function. `Block` around `Or` and `And` \
    are removed. *)
val sort: ('a -> 'a -> int) -> 'a formula -> 'a formula

(** Expressions composed entirely of version constraints *)
type version_formula = version_constraint formula

(** Checks if a given version satisfies a formula *)
val check_version_formula: version_formula -> OpamPackage.Version.t -> bool

(** An atom is: [name] * ([relop] * [version]) formula.
    Examples of valid formulae:
    - "foo" \{> "1" & (<"3" | ="5")\}
    - "foo" \{= "1" | > "4"\} | ("bar" "bouh") *)
type t = (OpamPackage.Name.t * version_formula) formula

val compare: t -> t -> int

(** Returns [true] if [package] verifies [formula] (i.e. it is within at least
    one package set that is a solution of the formula, and is named in the
    formula) *)
val verifies: t -> OpamPackage.t -> bool

(** Checks if a given set of (installed) packages satisfies a formula *)
val satisfies_depends: OpamPackage.Set.t -> t -> bool

(** Returns the subset of packages possibly matching the formula (i.e. including
    all disjunction cases) *)
val packages: OpamPackage.Set.t -> t -> OpamPackage.Set.t

val compare_nc:
  (OpamPackage.Name.t * version_formula) ->
  (OpamPackage.Name.t * version_formula) ->
  int

(** Convert a formula to CNF *)
val cnf_of_formula: 'a formula -> 'a formula

(** Convert a formula to CNF, but as a nested list *)
val formula_to_cnf: 'a formula -> 'a cnf

(** Convert a formula to DNF *)
val dnf_of_formula: 'a formula -> 'a formula

(** Convert a formula to DNF, but as a nested list *)
val formula_to_dnf: 'a formula -> 'a dnf

(** Transform a formula where versions can be expressed using formulas
    to a flat atom formula *)
val to_atom_formula: t -> atom formula

(** Convert an atom-formula to a t-formula *)
val of_atom_formula: atom formula -> t

(** [simplify_ineq_formula comp f] returns a canonical version of inequality
    formula [f], based on comparison function [comp], where each version appears
    at most once, and in increasing order. Returns [Some Empty] if the formula
    is always [true], [None] if it is always false *)
val simplify_ineq_formula:
  ('a -> 'a -> int) -> (relop * 'a) formula -> (relop * 'a) formula option

(** Like [simplify_ineq_formula], but specialised on version formulas *)
val simplify_version_formula: version_formula -> version_formula option

(** A more aggressive version of [simplify_version_formula] that attempts to
    find a shorter formula describing the same subset of versions within a given
    set. The empty formula is returned for an empty set, and the original
    formula is otherwise returned as is if no versions match. *)
val simplify_version_set: OpamPackage.Version.Set.t -> version_formula -> version_formula

(** [formula_of_version_set set subset] generates a formula that is enough to
    describe all packages of [subset] and exclude packages otherwise in [set] *)
val formula_of_version_set:
  OpamPackage.Version.Set.t -> OpamPackage.Version.Set.t -> version_formula

(** {2 Atoms} *)

(** Return all the atoms *)
val atoms: t -> atom list

(** Pretty print the formula *)
val to_string: t -> string

(** Return a conjunction. If the initial formula is not a
    conjunction, then fail. *)
val to_conjunction: t -> atom conjunction

(** Return a formula from a conjunction of atoms *)
val of_conjunction: atom conjunction -> t

(** Return a disjunction of atoms from a package formula. It the initial formula
    is not a disjunction, then fail. *)
val to_disjunction: t -> atom disjunction

(** Like [to_disjunction], but accepts conjunctions within constraint formulas,
    resolving them using the provided package set. Conjunctions between packages
    still raise [Failure]. *)
val set_to_disjunction: OpamPackage.Set.t -> t -> atom disjunction

(** Return a formula from a disjunction of atoms *)
val of_disjunction: atom disjunction -> t

(** Return an equivalent CNF formula *)
val to_cnf: t -> atom cnf

(** Return an equivalent DNF formula *)
val to_dnf: t -> atom dnf

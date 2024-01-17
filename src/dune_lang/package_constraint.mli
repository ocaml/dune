open! Stdune

module Value : sig
  type t =
    | String_literal of string
    | Variable of Package_variable_name.t
end

(** A constraint for use in package definitions, equivalent to the filter
    language used in opam package definitions. All expressions in this language
    are booleans. String literal values can only appear as the argument to an
    operator which returns a boolean. Variables are dynamically typed and can
    represent strings and booleans. If a variable appears in a position where a
    boolean is expected it will be assumed to represent a boolean. *)
type t =
  | Bvar of Package_variable_name.t (** A boolean variable *)
  | Uop of Relop.t * Value.t
  (** A unary operator applied to a value. Unary operators are operators
      whose LHS is implied by context. E.g. when placing version constraints
      on dependencies of a package the implied LHS is the version of the
      dependency: `(dependency (>= version))` *)
  | Bop of Relop.t * Value.t * Value.t
  (** A binary operator applied to LHS and RHS values *)
  | And of t list (** The conjunction of a list of boolean expressions *)
  | Or of t list (** The disjunction of a list of boolean expressions *)

val encode : t Dune_sexp.Encoder.t
val decode : t Dune_sexp.Decoder.t
val to_dyn : t -> Dyn.t
val equal : t -> t -> bool

include Comparable_intf.S with type key := t

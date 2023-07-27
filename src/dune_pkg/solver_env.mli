open! Stdune

(** A typed variable environment used by the dependency solver to evaluate
    package filters. Opam packages can declare conditional dependencies on other
    packages using a language made up of boolean operators and comparisons of
    strings. Variables in this language can represent booleans and strings. *)

module Flag : sig
  (** A boolean variable *)
  type t =
    [ `With_test
    | `With_doc
    ]

  val to_string : t -> string

  val of_string_opt : string -> t option

  module Set : sig
    type flag := t

    (** A set flags. The presence of a flag in the set indicates that the flag
        is set to true. *)
    type t

    val fold : t -> init:'a -> f:(flag -> 'a -> 'a) -> 'a

    val mem : t -> flag -> bool
  end
end

module Sys_var : sig
  (** A system environment variable. Each of these variables may be assigned a
      string value. If system environment variable has a value then that
      variable binding will be used when evaluating opam dependency filters.
      Unset variables are treated as wildcards. *)
  type t =
    [ `Arch
    | `Os
    | `Os_version
    | `Os_distribution
    | `Os_family
    | `Opam_version
    ]

  val to_string : t -> string

  val of_string_opt : string -> t option

  module Bindings : sig
    type sys_var := t

    (** A mapping from system environment variables to their values *)
    type t

    val empty : t

    val set : t -> sys_var -> string -> t

    val get : t -> sys_var -> string option

    type union_error =
      [ `Var_in_both_with_different_values of sys_var * string * string ]

    (** Merge two environments returning an error if they both contain a binding
        of the same variable to different values. *)
    val union : t -> t -> (t, union_error) result
  end
end

type t =
  { flags : Flag.Set.t
  ; sys : Sys_var.Bindings.t
  }

val decode : t Dune_lang.Decoder.t

val encode : t Dune_lang.Encoder.t

val to_dyn : t -> Dyn.t

val equal : t -> t -> bool

val default : t

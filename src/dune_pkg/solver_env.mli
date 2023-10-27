open Import

module Variable : sig
  module Sys : sig
    (** Variables describing a system environment. These can be polled from the
        current system and assigned specific values in dune-workspace but there
        is no notion of a default value. During solving unset variables are
        treated as wildcards so a solution can be built that works on a range of
        systems. During building unset variables are treated as undefined
        variables. *)
    type t =
      [ `Arch
      | `Os
      | `Os_version
      | `Os_distribution
      | `Os_family
      ]

    val to_string : t -> string
    val of_string_opt : string -> t option

    module Bindings : sig
      type sys_var := t

      (** A mapping from system environment variables to their values *)
      type t

      val to_dyn : t -> Dyn.t
      val decode : t Decoder.t
      val equal : t -> t -> bool
      val empty : t
      val set : t -> sys_var -> string -> t
      val get : t -> sys_var -> string option

      (** [extend a b] adds all variables from [b] to [a] overwriting any
          existing values of those variables in [a]. *)
      val extend : t -> t -> t
    end
  end

  module Const : sig
    (** Constant variables whose value can't be configured. These can be read
        while solving and evaluating packages. *)
    type t = [ `Opam_version ]
  end

  type t =
    | Sys of Sys.t
    | Const of Const.t

  val to_string : t -> string
  val of_string_opt : string -> t option

  include Comparable_intf.S with type key := t

  val equal : t -> t -> bool
  val to_dyn : t -> Dyn.t
  val decode : t Decoder.t
  val encode : t Encoder.t
end

(** A variable environment used by the dependency solver to evaluate package
    dependency filters. Opam packages can declare conditional dependencies on
    other packages using a language made up of boolean operators and comparisons
    of strings. Note that the variables in this environment are those that dune
    allows users to configure or derive from their environment. Other variables,
    such as the flags "with-test" and "with-doc" are not part of this
    environment as dune does not give users access to those variables.. *)
type t

val create : sys:Variable.Sys.Bindings.t -> t
val default : t
val decode : t Decoder.t
val to_dyn : t -> Dyn.t
val equal : t -> t -> bool
val sys : t -> Variable.Sys.Bindings.t
val set_sys : t -> Variable.Sys.Bindings.t -> t

(** A human-readible summary of the variable environment *)
val pp : t -> 'a Pp.t

module Variable_value : sig
  type t =
    | String of string
    | Unset_sys
end

val get : t -> Variable.t -> Variable_value.t

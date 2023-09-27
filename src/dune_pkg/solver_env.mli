open! Stdune

module Variable : sig
  module Flag : sig
    (** A boolean variable *)
    type t =
      [ `With_test
      | `With_doc
      ]
  end

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

      val empty : t
      val set : t -> sys_var -> string -> t
      val get : t -> sys_var -> string option

      type union_error =
        [ `Var_in_both_with_different_values of sys_var * string * string ]

      (** Merge two environments returning an error if they both contain a
          binding of the same variable to different values. *)
      val union : t -> t -> (t, union_error) result

      val encode : t -> Dune_sexp.t list
    end
  end

  module Const : sig
    (** Constant variables whose value can't be configured. These can be read
        while solving and evaluating packages. *)
    type t = [ `Opam_version ]
  end

  type t =
    | Flag of Flag.t
    | Sys of Sys.t
    | Const of Const.t

  val of_string_opt : string -> t option
end

(** A typed variable environment used by the dependency solver to evaluate
    package filters. Opam packages can declare conditional dependencies on other
    packages using a language made up of boolean operators and comparisons of
    strings. Variables in this language can represent booleans and strings. *)
type t

val default : t
val decode : t Dune_sexp.Decoder.t
val to_dyn : t -> Dyn.t
val equal : t -> t -> bool
val sys : t -> Variable.Sys.Bindings.t
val set_sys : t -> Variable.Sys.Bindings.t -> t

(** [repos t] returns the selected repository names in priority order *)
val repos : t -> Workspace.Repository.Name.t list

(** Set all the flags to false *)
val clear_flags : t -> t

(** A human-readible summary of the variable environment *)
val pp : t -> 'a Pp.t

module Variable_value : sig
  type t =
    | Bool of bool
    | String of string
    | Unset_sys
end

val get : t -> Variable.t -> Variable_value.t

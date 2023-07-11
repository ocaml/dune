open! Stdune

module Flag : sig
  type t =
    [ `With_test
    | `With_doc
    ]

  val to_string : t -> string

  val of_string_opt : string -> t option

  module Set : sig
    type flag := t

    type t

    val fold : t -> init:'a -> f:(flag -> 'a -> 'a) -> 'a

    val mem : t -> flag -> bool
  end
end

module Sys_var : sig
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

    type t

    val empty : t

    val set : t -> sys_var -> string -> t

    val get : t -> sys_var -> string option

    type merge_error =
      [ `Var_in_both_with_different_values of sys_var * string * string ]

    (** Merge two environments returning an error if they both contain a binding
        of the same variable to different values. *)
    val merge : t -> t -> (t, merge_error) result
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

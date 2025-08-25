open Import

module Ppx_args : sig
  module Cookie : sig
    type t =
      { name : string
      ; value : String_with_vars.t
      }
  end

  type t = { cookies : Cookie.t list }

  val empty : t
end

module Dune_file : sig
  (** Representation of of valid dune lang values for the library [kind]
      field *)
  type t =
    | Normal
    | Parameter
    | Ppx_deriver of Ppx_args.t
    | Ppx_rewriter of Ppx_args.t

  (** [cstr_name k] is the name of the kind constructor for [k] in dune lang.
      E.g., [cstr_name Normal = "normal"] *)
  val cstr_name : t -> string

  val equal : t -> t -> bool

  include Conv.S with type t := t
end

(** Internal representation the of the possible kinds of libraries *)
type t =
  | Dune_file of Dune_file.t
  | Virtual

val to_dyn : t Dyn.builder
val equal : t -> t -> bool

include Conv.S with type t := t

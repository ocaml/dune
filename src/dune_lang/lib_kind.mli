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
    | Ppx_deriver of Ppx_args.t
    | Ppx_rewriter of Ppx_args.t

  (** [cstr_name k] is the name of the kind constructor for [k] in dune lang.
      E.g., [cstr_name Normal = "normal"] *)
  val cstr_name : t -> string

  include Conv.S with type t := t
end

(** Internal representation the of the possible kinds of libraries *)
type t =
  | Virtual
  | Parameter
  | Dune_file of Dune_file.t
  (** A kind which is represented explicitly in the [kind] field of a dune
      library stanza.

      The remaining variants are derived from other fields in the library
      stanza. *)

val to_dyn : t Dyn.builder

include Conv.S with type t := t

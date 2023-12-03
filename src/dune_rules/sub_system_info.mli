open Import

(** The type of all kind of sub-system information. This type is what we get
    just after parsing a [dune] file. *)
type t = ..

type sub_system = t = ..

(** What the user must provide in order to define the parsing part of a
    sub-system. *)
module type S = sig
  type t
  type sub_system += T of t

  (** Name of the sub-system *)
  val name : Sub_system_name.t

  (** Location of the parameters in the [jbuild] or [dune] file. *)
  val loc : t -> Loc.t

  (** Syntax for [jbuild]/[dune] files *)
  val syntax : Dune_lang.Syntax.t

  (** Parse parameters written by the user in [jbuild]/[dune] files *)
  val decode : t Dune_lang.Decoder.t

  (** Dump the sub-system configuration. This is used to generate dune-package
      files. *)
  val encode : t -> Dune_lang.Syntax.Version.t * Dune_lang.t list
end

module Register (_ : S) : sig end

val record_parser : t Sub_system_name.Map.t Dune_lang.Decoder.fields_parser
val get : Sub_system_name.t -> (module S)
val equal : t -> t -> bool

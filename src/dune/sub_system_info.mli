open Stdune

(** The type of all kind of sub-system information.
    This type is what we get just after parsing a [jbuild] file. *)
type t = ..
type sub_system = t = ..

(** What the user must provide in order to define the parsing part
    of a sub-system. *)
module type S = sig
  type t
  type sub_system += T of t

  (** Name of the sub-system *)
  val name : Sub_system_name.t

  (** Location of the parameters in the jbuild/dune file. *)
  val loc : t -> Loc.t

  (** Syntax for jbuild/dune files *)
  val syntax : Syntax.t

  (** Parse parameters written by the user in jbuid/dune files *)
  val parse : t Dune_lang.Decoder.t
end

module Register(M : S) : sig end

val record_parser
  :  unit
  -> (sub_system Sub_system_name.Map.t, Dune_lang.Decoder.fields)
       Dune_lang.Decoder.parser

val get : Sub_system_name.t -> (module S)

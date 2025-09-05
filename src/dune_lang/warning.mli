(** General warning mechanism for dune rules *)

open Import
open Dune_config

type t

val make
  :  default:(Syntax.Version.t -> Config.Toggle.t)
  -> name:string
  -> since:Syntax.Version.t
  -> t

val name : t -> string

module Settings : sig
  (** Settings to disable/enable specific warnings in a project *)

  type warning := t
  type t

  val to_dyn : t -> Dyn.t
  val empty : t
  val decode : t Decoder.t
  val active : t -> warning -> Syntax.Version.t -> Config.Toggle.t
end

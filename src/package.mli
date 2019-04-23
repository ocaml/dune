(** Information about a package defined in the workspace *)

open! Stdune

module Name : sig
  type t

  val of_string : string -> t

  val opam_fn : t -> string

  val version_fn : t -> string

  include Interned.S with type t := t

  include Dune_lang.Conv with type t := t

  module Infix : Comparable.OPS with type t = t
end

type t =
  { name                   : Name.t
  ; path                   : Path.Source.t
  ; version_from_opam_file : string option
  }

val pp : Format.formatter -> t -> unit

val opam_file : t -> Path.Source.t

val meta_file : t -> Path.Source.t

val to_dyn : t -> Dyn.t

val hash : t -> int

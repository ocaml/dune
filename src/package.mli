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

module Version_source : sig
  (** Wether this version comes from the project wide version or the
      package particular version *)
  type t =
    | Package
    | Project
end

type t =
  { name    : Name.t
  ; path    : Path.Source.t
  ; version : (string * Version_source.t) option
  }

val pp : Format.formatter -> t -> unit

val opam_file : t -> Path.Source.t

val meta_file : t -> Path.Source.t

val to_dyn : t -> Dyn.t

val hash : t -> int

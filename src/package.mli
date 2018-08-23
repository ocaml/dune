(** Information about a package defined in the workspace *)

open! Stdune

module Name : sig
  type t

  val of_string : string -> t

  val opam_fn : t -> string

  include Interned.S with type t := t

  val dparse : t Dsexp.Of_sexp.t

  module Infix : Comparable.OPS with type t = t
end

type t =
  { name                   : Name.t
  ; path                   : Path.t
  ; version_from_opam_file : string option
  }

val opam_file : t -> Path.t

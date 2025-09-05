open Stdune

module Expanded : sig
  type t

  val to_dyn : t -> Dyn.t
  val src : t -> Path.Build.t
  val dst : t -> string option
  val dir : t -> Path.Source.t option
  val dst_with_loc : t -> (Loc.t * string) option
  val src_loc : t -> Loc.t
  val dst_path : t -> dir:Path.Build.t -> Path.Build.t
end

module Unexpanded : sig
  type t

  val src : t -> String_with_vars.t
  val dst : t -> String_with_vars.t option
  val dir : t -> Path.Source.t option
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
  val loc : t -> Loc.t

  val expand
    :  t
    -> dir:Path.Source.t
    -> src:Loc.t * Path.Build.t
    -> dst:(Loc.t * string) option
    -> Expanded.t

  val make
    :  src:Loc.t * string
    -> dst:Loc.t * string
    -> dune_syntax:Dune_sexp.Syntax.Version.t
    -> dir:Path.Source.t option
    -> t

  val decode : t Dune_sexp.Decoder.t
  val dune_syntax : t -> Dune_sexp.Syntax.Version.t

  module L : sig
    val decode : t list Dune_sexp.Decoder.t

    (** Determine if there is a pform in the unexpanded source. If there is one,
        return a loc appropriate for an error message. Otherwise, return [None]. *)
    val find_pform : t list -> Loc.t option
  end
end

open! Dune_engine
open Stdune

module Expanded : sig
  type t

  val src : t -> Path.Build.t

  val dst : t -> string option

  val src_loc : t -> Loc.t

  val dst_path : t -> dir:Path.Build.t -> Path.Build.t
end

module Unexpanded : sig
  type t

  val equal : t -> t -> bool

  val make : src:Loc.t * string -> dst:Loc.t * string -> t

  val expand :
    t -> dir:Path.Build.t -> f:(String_with_vars.t -> string) -> Expanded.t

  val expand_src :
    t -> dir:Path.Build.t -> f:(String_with_vars.t -> string) -> Path.Build.t

  val destination_relative_to_install_path :
       t
    -> section:Install.Section.t
    -> expand:(String_with_vars.t -> string)
    -> expand_partial:(String_with_vars.t -> string String_with_vars.Partial.t)
    -> Install.Dst.t

  module L : sig
    val decode : t list Dune_lang.Decoder.t
  end
end

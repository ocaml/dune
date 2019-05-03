open Stdune

module Expanded : sig
  type t

  val src : t -> Path.t
  val dst : t -> Path.Local.t option

  val src_loc : t -> Loc.t

  val dst_path : t -> dir:Path.t -> Path.t
  val src_path : t -> Path.t
end

module Unexpanded : sig
  type t

  val make : src:(Loc.t * string) -> dst:(Loc.t * string) -> t

  val expand
    :  t
    -> dir:Path.t
    -> f:(String_with_vars.t -> string)
    -> Expanded.t

  val expand_src
    : t -> dir:Path.t -> f:(String_with_vars.t -> string) -> Path.t

  val destination_relative_to_install_path
    : t
    -> section:Install.Section.t
    -> expand:(String_with_vars.t -> string)
    -> expand_partial:(String_with_vars.t -> string String_with_vars.Partial.t)
    -> Install.Dst.t

  module L : sig
    val decode : t list Stanza.Decoder.t
  end
end

open Stdune

module Expanded : sig
  type t

  val src : t -> string
  val dst : t -> string option

  val src_loc : t -> Loc.t

  val dst_path : t -> dir:Path.t -> Path.t
  val src_path : t -> dir:Path.t -> Path.t
end

module Unexpanded : sig
  type t

  val make : src:(Loc.t * string) -> dst:(Loc.t * string) -> t

  val expand : t -> f:(String_with_vars.t -> string) -> Expanded.t

  val expand_src : t -> f:(String_with_vars.t -> string) -> string

  module L : sig
    val decode : t list Stanza.Decoder.t
  end
end

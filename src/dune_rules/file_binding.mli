open Import

module Expanded : sig
  type t

  val to_dyn : t -> Dyn.t

  val src : t -> Path.Build.t

  val dst : t -> string option

  val src_loc : t -> Loc.t

  val dst_path : t -> dir:Path.Build.t -> Path.Build.t
end

module Unexpanded : sig
  type t

  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool

  val make : src:Loc.t * string -> dst:Loc.t * string -> t

  val decode : t Dune_lang.Decoder.t

  val expand :
       t
    -> dir:Path.Build.t
    -> f:(String_with_vars.t -> string Memo.t)
    -> Expanded.t Memo.t

  val expand_src :
       t
    -> dir:Path.Build.t
    -> f:(String_with_vars.t -> string Memo.t)
    -> Path.Build.t Memo.t

  val destination_relative_to_install_path :
       t
    -> section:Install.Section.t
    -> expand:(String_with_vars.t -> string Memo.t)
    -> expand_partial:(String_with_vars.t -> String_with_vars.t Memo.t)
    -> Install.Dst.t Memo.t

  module L : sig
    val decode : t list Dune_lang.Decoder.t

    (** Determine if there is a pform in the unexpanded source. If there is one,
        return a loc appropriate for an error message. Otherwise, return [None]. *)
    val find_pform : t list -> Loc.t option
  end
end

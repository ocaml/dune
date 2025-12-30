(** Opam install file *)

open Import

module Dst : sig
  type t

  val to_string : t -> string
  val add_prefix : string -> t -> t
  val add_suffix : t -> string -> t
  val concat_all : t -> string list -> t

  include Dune_lang.Conv.S with type t := t

  val to_dyn : t -> Dyn.t
  val install_path : Path.t Paths.t -> Section.t -> t -> Path.t
end

type ('src, 'kind) t = private
  { src : 'src
  ; kind : 'kind
  ; dst : Dst.t
  ; section : Section.t
  ; optional : bool
  }

module Expanded : sig
  type kind =
    | File
    | Directory

  type nonrec 'src t = ('src, kind) t

  val set_src : _ t -> 'src -> 'src t
  val add_install_prefix : 'src t -> paths:Path.t Paths.t -> prefix:Path.t -> 'src t
  val gen_install_file : Path.t t list -> string
  val load_install_file : Path.t -> (Path.Local.t -> Path.t) -> Path.t t list
end

module Unexpanded : sig
  type kind =
    | File
    | Directory
    | Source_tree

  type nonrec t = (Path.Build.t, kind) t

  val make : Section.t -> ?dst:string -> kind:kind -> Path.Build.t -> t
  val make_with_dst : Section.t -> Dst.t -> kind:kind -> src:Path.Build.t -> t
  val expand : t -> Path.Build.t Expanded.t
  val compare : t -> t -> Ordering.t
end

module Sourced : sig
  type source =
    | User of Loc.t
    | Dune

  type nonrec 'entry t =
    { source : source
    ; entry : 'entry
    }

  module Unexpanded : sig
    type nonrec t = Unexpanded.t t

    val create : ?loc:Loc.t -> Unexpanded.t -> t
    val to_dyn : t -> Dyn.t
  end

  module Expanded : sig
    type entry := Path.Build.t Expanded.t
    type nonrec t = entry t
  end
end

val adjust_dst
  :  src:Dune_lang.String_with_vars.t
  -> dst:string option
  -> section:Section.t
  -> Dst.t

val adjust_dst' : src:Path.Build.t -> dst:string option -> section:Section.t -> Dst.t
val map_dst : ('src, 'kind) t -> f:(Dst.t -> Dst.t) -> ('src, 'kind) t
val relative_installed_path : _ t -> paths:Path.t Paths.t -> Path.t

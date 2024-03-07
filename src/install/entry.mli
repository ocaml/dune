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

type kind =
  [ `File
  | `Directory
  | `Source_tree
  ]

type 'src t = private
  { src : 'src
  ; kind : kind
  ; dst : Dst.t
  ; section : Section.t
  ; optional : bool
  }

module Sourced : sig
  type source =
    | User of Loc.t
    | Dune

  type entry := Path.Build.t t

  type nonrec t =
    { source : source
    ; entry : entry
    }

  val create : ?loc:Loc.t -> entry -> t
  val to_dyn : t -> Dyn.t
end

val adjust_dst
  :  src:Dune_lang.String_with_vars.t
  -> dst:string option
  -> section:Section.t
  -> Dst.t

val set_kind : 'src t -> kind -> 'src t
val adjust_dst' : src:Path.Build.t -> dst:string option -> section:Section.t -> Dst.t
val make : Section.t -> ?dst:string -> kind:kind -> Path.Build.t -> Path.Build.t t
val make_with_dst : Section.t -> Dst.t -> kind:kind -> src:Path.Build.t -> Path.Build.t t
val set_src : _ t -> 'src -> 'src t
val map_dst : 'a t -> f:(Dst.t -> Dst.t) -> 'a t
val relative_installed_path : _ t -> paths:Path.t Paths.t -> Path.t
val add_install_prefix : 'a t -> paths:Path.t Paths.t -> prefix:Path.t -> 'a t
val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t
val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
val gen_install_file : Path.t t list -> string
val load_install_file : Path.t -> (Path.Local.t -> Path.t) -> Path.t t list

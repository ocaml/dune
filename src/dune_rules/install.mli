(** Opam install file *)
open Import

module Dst : sig
  type t

  val to_string : t -> string

  val concat_all : t -> string list -> t

  include Dune_lang.Conv.S with type t := t

  val to_dyn : t -> Dyn.t
end

(** Location for installation, containing the sections relative to the current
    package, and sites of possibly other packages *)
module Section_with_site : sig
  type t =
    | Section of Dune_engine.Section.t
    | Site of
        { pkg : Dune_engine.Package.Name.t
        ; site : Dune_engine.Section.Site.t
        ; loc : Loc.t
        }

  val to_string : t -> string

  (* val parse_string : string -> (t, string) Result.t *)

  include Dune_lang.Conv.S with type t := t

  val to_dyn : t -> Dyn.t
end

module Section : sig
  type t = Dune_engine.Section.t

  include Comparable_intf.S with type key := t

  val to_string : t -> string

  val parse_string : string -> (t, string) Result.t

  val decode : t Dune_lang.Decoder.t

  val to_dyn : t -> Dyn.t

  module Paths : sig
    module Roots : sig
      type 'a t =
        { lib_root : 'a
        ; libexec_root : 'a
        ; bin : 'a
        ; sbin : 'a
        ; share_root : 'a
        ; etc_root : 'a
        ; doc_root : 'a
        ; man : 'a
        }

      (** Compute the opam layout from prefix. the opam layout is used for
          _build *)
      val opam_from_prefix : Path.t -> Path.t t

      (** Some roots (e.g. libexec) have another roots as default (e.g. lib) *)
      val complete : 'a option t -> 'a option t

      val map : f:('a -> 'b) -> 'a t -> 'b t

      (** return the roots of the first argument if present *)
      val first_has_priority : 'a option t -> 'a option t -> 'a option t
    end

    type section := t

    type t

    val make : package:Dune_engine.Package.Name.t -> roots:Path.t Roots.t -> t

    val install_path : t -> section -> Dst.t -> Path.t

    val get : t -> section -> Path.t

    val get_local_location :
         Dune_engine.Context_name.t
      -> section
      -> Dune_engine.Package.Name.t
      -> Path.t
  end
end

module Entry : sig
  type 'src t = private
    { src : 'src
    ; kind : [ `File | `Directory ]
    ; dst : Dst.t
    ; section : Section.t
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
  end

  val adjust_dst :
       src:Dune_lang.String_with_vars.t
    -> dst:string option
    -> section:Section.t
    -> Dst.t

  val make :
       Section.t
    -> ?dst:string
    -> kind:[ `File | `Directory ]
    -> Path.Build.t
    -> Path.Build.t t

  val make_with_site :
       Section_with_site.t
    -> ?dst:string
    -> (   loc:Loc.t
        -> pkg:Dune_engine.Package.Name.t
        -> site:Dune_engine.Section.Site.t
        -> Section.t Memo.t)
    -> kind:[ `File | `Directory ]
    -> Path.Build.t
    -> Path.Build.t t Memo.t

  val set_src : _ t -> 'src -> 'src t

  val map_dst : 'a t -> f:(Dst.t -> Dst.t) -> 'a t

  val relative_installed_path : _ t -> paths:Section.Paths.t -> Path.t

  val add_install_prefix :
    'a t -> paths:Section.Paths.t -> prefix:Path.t -> 'a t

  val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t
end

(** Same as Entry, but the destination can be in the site of a package *)
module Entry_with_site : sig
  type 'src t =
    { src : 'src
    ; dst : Dst.t
    ; section : Section_with_site.t
    }
end

val gen_install_file : Path.t Entry.t list -> string

(** XXX what's this function doing here? it has nothing to do with generating
    any rules *)
val load_install_file : Path.t -> Path.t Entry.t list

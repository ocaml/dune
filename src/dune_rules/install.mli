(** Opam install file *)
open! Dune_engine

open! Stdune

module Dst : sig
  type t

  val to_string : t -> string
end

module Section : sig
  type t =
    | Lib
    | Lib_root
    | Libexec
    | Libexec_root
    | Bin
    | Sbin
    | Toplevel
    | Share
    | Share_root
    | Etc
    | Doc
    | Stublibs
    | Man
    | Misc

  module Set : Set.S with type elt = t

  val all : Set.t

  val to_string : t -> string

  val parse_string : string -> (t, string) Result.t

  val decode : t Dune_lang.Decoder.t

  (** [true] iff the executable bit should be set for files installed in this
      location. *)
  val should_set_executable_bit : t -> bool

  module Paths : sig
    type section = t

    type t

    val make :
         package:Package.Name.t
      -> destdir:Path.t
      -> ?libdir:Path.t
      -> ?mandir:Path.t
      -> unit
      -> t

    val install_path : t -> section -> Dst.t -> Path.t
  end
  with type section := t
end

module Entry : sig
  type 'src t = private
    { src : 'src
    ; dst : Dst.t
    ; section : Section.t
    }

  val adjust_dst :
       src:string String_with_vars.Partial.t
    -> dst:string option
    -> section:Section.t
    -> Dst.t

  val make : Section.t -> ?dst:string -> Path.Build.t -> Path.Build.t t

  val set_src : _ t -> 'src -> 'src t

  val relative_installed_path : _ t -> paths:Section.Paths.t -> Path.t

  val add_install_prefix :
    Path.Build.t t -> paths:Section.Paths.t -> prefix:Path.t -> Path.Build.t t
end

val files : Path.Build.t Entry.t list -> Path.Set.t

val gen_install_file : Path.Build.t Entry.t list -> string

val load_install_file : Path.t -> Path.t Entry.t list

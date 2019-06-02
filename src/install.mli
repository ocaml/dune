(** Opam install file *)

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

  val decode : t Dune_lang.Decoder.t

  (** [true] iff the executable bit should be set for files installed
      in this location. *)
  val should_set_executable_bit : t -> bool

  module Paths : sig
    type section = t

    type t = private
      { lib          : Path.t
      ; lib_root     : Path.t
      ; libexec      : Path.t
      ; libexec_root : Path.t
      ; bin          : Path.t
      ; sbin         : Path.t
      ; toplevel     : Path.t
      ; share        : Path.t
      ; share_root   : Path.t
      ; etc          : Path.t
      ; doc          : Path.t
      ; stublibs     : Path.t
      ; man          : Path.t
      }

    val make
      :  package:Package.Name.t
      -> destdir:Path.t
      -> ?libdir:Path.t
      -> unit
      -> t

    val install_path : t -> section -> Dst.t -> Path.t
  end with type section := t
end

module Entry : sig

  type t = private
    { src     : Path.Build.t
    ; dst     : Dst.t
    ; section : Section.t
    }

  val adjust_dst :
    src:(string String_with_vars.Partial.t)
    -> dst:string option
    -> section:Section.t
    -> Dst.t

  val make : Section.t -> ?dst:string -> Path.Build.t -> t
  val set_src : t -> Path.Build.t -> t

  val relative_installed_path : t -> paths:Section.Paths.t -> Path.t
  val add_install_prefix : t -> paths:Section.Paths.t -> prefix:Path.t -> t
end

val files : Entry.t list -> Path.Set.t
val gen_install_file : Entry.t list -> string

val load_install_file : Path.t -> Entry.t list

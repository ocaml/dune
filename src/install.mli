(** Opam install file *)

module Section : sig
  type t =
    | Lib
    | Libexec
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

  val t : t Sexp.Of_sexp.t
end

module Entry : sig
  type t =
    { src     : Path.t
    ; dst     : string option
    ; section : Section.t
    }

  val make : Section.t -> ?dst:string -> Path.t -> t

  val relative_installed_path : t -> package:string -> Path.t
end

val files : Entry.t list -> Path.Set.t
val write_install_file : Path.t -> Entry.t list -> unit

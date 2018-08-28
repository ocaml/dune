open! Stdune
open! Import

module Name : sig
  type t

  include Dsexp.Sexpable with type t := t

  val add_suffix : t -> string -> t

  val to_sexp : t Sexp.To_sexp.t
  val compare : t -> t -> Ordering.t
  val of_string : string -> t
  val to_string : t -> string

  val uncapitalize : t -> string

  val pp : Format.formatter -> t -> unit
  val pp_quote : Format.formatter -> t -> unit

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t

  module Top_closure : Top_closure.S with type key := t

  module Infix : Comparable.OPS with type t = t
end

module Syntax : sig
  type t = OCaml | Reason
end

module File : sig
  type t =
    { path   : Path.t
    ; syntax : Syntax.t
    }

  val make : Syntax.t -> Path.t -> t
end

(** Representation of a module. It is guaranteed that at least one of
   [impl] or [intf] is set. *)
type t = private
  { name      : Name.t (** Name of the module. This is always the
                           basename of the filename without the
                           extension. *)
  ; impl      : File.t option
  ; intf      : File.t option
  ; obj_name  : string (** Object name. It is different from [name]
                           for wrapped modules. *)
  ; pp        : (unit, string list) Build.t option (** Preprocessing flags *)
  }

val make
  :  ?impl:File.t
  -> ?intf:File.t
  -> ?obj_name:string
  -> Name.t
  -> t

val name : t -> Name.t

(** Real unit name once wrapped. This is always a valid module name. *)
val real_unit_name : t -> Name.t

val file      : t -> Ml_kind.t -> Path.t option
val cm_source : t -> Cm_kind.t -> Path.t option
val cm_file   : t -> obj_dir:Path.t -> Cm_kind.t -> Path.t option
val cmt_file  : t -> obj_dir:Path.t -> Ml_kind.t -> Path.t option

val obj_file : t -> obj_dir:Path.t -> ext:string -> Path.t

val dir : t -> Path.t

(** Same as [cm_file] but doesn't raise if [cm_kind] is [Cmo] or [Cmx]
    and the module has no implementation. *)
val cm_file_unsafe : t -> obj_dir:Path.t -> Cm_kind.t -> Path.t

val odoc_file : t -> doc_dir:Path.t -> Path.t

(** Either the .cmti, or .cmt if the module has no interface *)
val cmti_file : t -> obj_dir:Path.t -> Path.t

val iter : t -> f:(Ml_kind.t -> File.t -> unit) -> unit

val has_impl : t -> bool

(** Prefix the object name with the library name. *)
val with_wrapper : t -> libname:Lib_name.Local.t -> t

val map_files : t -> f:(Ml_kind.t -> File.t -> File.t) -> t

val set_pp : t -> (unit, string list) Build.t option -> t

open! Stdune
open! Import

module Name : sig
  type t

  include Dune_lang.Conv with type t := t

  val add_suffix : t -> string -> t

  val to_sexp : t Sexp.Encoder.t
  val compare : t -> t -> Ordering.t
  val of_string : string -> t
  val to_string : t -> string

  val uncapitalize : t -> string

  (** Split a prefixed module name into its components *)
  val split_alias_prefix : t -> (t * t) option

  val pp : Format.formatter -> t -> unit
  val pp_quote : Format.formatter -> t -> unit

  module Set : sig
    include Set.S with type elt = t
    val pp : t Fmt.t
  end

  module Map : Map.S with type key = t

  module Top_closure : Top_closure.S
    with type key := t
     and type 'a monad := 'a Monad.Id.t

  module Infix : Comparable.OPS with type t = t

  val of_local_lib_name : Lib_name.Local.t -> t

  val to_local_lib_name : t -> Lib_name.Local.t
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

module Visibility : sig
  type t = Public | Private

  include Dune_lang.Conv with type t := t
end

module Kind : sig
  type t = Intf_only | Virtual | Impl

  include Dune_lang.Conv with type t := t
end

type t

(** [obj_name] Object name. It is different from [name] for wrapped modules. *)
val make
  :  ?impl:File.t
  -> ?intf:File.t
  -> ?obj_name:string
  -> visibility:Visibility.t
  -> obj_dir:Obj_dir.t
  -> kind:Kind.t
  -> Name.t
  -> t

val name : t -> Name.t

(** Real unit name once wrapped. This is always a valid module name. *)
val real_unit_name : t -> Name.t

val intf : t -> File.t option
val impl : t -> File.t option
val obj_dir : t -> Obj_dir.t

val pp_flags : t -> (unit, string list) Build.t option

val file            : t -> Ml_kind.t -> Path.t option
val cm_source       : t -> Cm_kind.t -> Path.t option
val cm_file         : t -> ?ext:string -> Cm_kind.t -> Path.t option
val cm_public_file  : t -> ?ext:string -> Cm_kind.t -> Path.t option
val cmt_file        : t -> Ml_kind.t -> Path.t option

val obj_file : t -> mode:Mode.t -> ext:string -> Path.t

val obj_name : t -> string

val src_dir : t -> Path.t option

(** Same as [cm_file] but doesn't raise if [cm_kind] is [Cmo] or [Cmx]
    and the module has no implementation.
    If present [ext] replace the extension of the kind
 *)
val cm_file_unsafe : t -> ?ext:string -> Cm_kind.t -> Path.t
val cm_public_file_unsafe : t -> ?ext:string -> Cm_kind.t -> Path.t

val odoc_file : t -> doc_dir:Path.t -> Path.t

(** Either the .cmti, or .cmt if the module has no interface *)
val cmti_file : t -> Path.t

val iter : t -> f:(Ml_kind.t -> File.t -> unit) -> unit

val has_impl : t -> bool
val has_intf : t -> bool
val impl_only : t -> bool
val intf_only : t -> bool

(** Prefix the object name with the library name. *)
val with_wrapper : t -> main_module_name:Name.t -> t

val map_files : t -> f:(Ml_kind.t -> File.t -> File.t) -> t

(** Set preprocessing flags *)
val set_pp : t -> (unit, string list) Build.t option -> t

val to_sexp : t Sexp.Encoder.t

val pp : t Fmt.t

val wrapped_compat : t -> t

module Name_map : sig
  type module_
  type t = module_ Name.Map.t

  val impl_only : t -> module_ list

  val of_list_exn : module_ list -> t

  val add : t -> module_ -> t
end with type module_ := t

module Obj_map : sig
  type module_
  include Map.S with type key = module_

  val top_closure
    :  module_ list t
    -> module_ list
    -> (module_ list, module_ list) Result.result
end with type module_ := t

val is_public : t -> bool
val is_private : t -> bool
val is_virtual : t -> bool

val set_private : t -> t
val set_obj_dir : t -> obj_dir:Obj_dir.t -> t
val set_virtual : t -> t

val remove_files : t -> t

val sources : t -> Path.t list

val visibility : t -> Visibility.t

val encode : t -> Dune_lang.t list

val decode : dir:Path.t -> t Dune_lang.Decoder.t

(* Only the source of a module, not yet associated to a library *)
module Source : sig
  type t = private
    { name : Name.t
    ; impl : File.t option
    ; intf : File.t option
    }

  val make
    :  ?impl:File.t
    -> ?intf:File.t
    -> Name.t
    -> t

  val has_impl: t -> bool

  val src_dir : t -> Path.t option

  val name : t -> Name.t
end

open Import

(** {1 Generals} *)

(** Representation of a library *)
type t

val to_dyn : t -> Dyn.t

(** For libraries defined in the workspace, this is the [public_name] if present
    or the [name] if not. *)
val name : t -> Lib_name.t

val implements : t -> t Resolve.Memo.t option

(** [is_local t] returns [true] whenever [t] is defined in the local workspace *)
val is_local : t -> bool

val info : t -> Path.t Lib_info.t
val main_module_name : t -> Module_name.t option Resolve.Memo.t
val wrapped : t -> Wrapped.t option Resolve.Memo.t

(** Direct library dependencies of this library *)
val requires : t -> t list Resolve.Memo.t

val re_exports : t -> t list Resolve.Memo.t
val ppx_runtime_deps : t -> t list Resolve.Memo.t
val pps : t -> t list Resolve.Memo.t

include Comparable_intf.S with type key := t

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val hash : t -> int
val project : t -> Dune_project.t option

(** Operations on list of libraries *)
module L : sig
  val top_closure
    :  'a list
    -> key:('a -> t)
    -> deps:('a -> 'a list Resolve.Memo.t)
    -> ('a list, 'a list) Result.t Resolve.Memo.t
end

(** {1 Compilation contexts} *)

(** See {!Sub_system} *)
type sub_system = ..

type db

(** For compiling a library or executable *)
module Compile : sig
  type lib := t
  type t

  val for_lib : allow_overlaps:bool -> db -> lib -> t

  (** Return the list of dependencies needed for linking this library/exe *)
  val requires_link : t -> lib list Resolve.t Memo.Lazy.t

  (** Dependencies listed by the user + runtime dependencies from ppx *)
  val direct_requires : t -> lib list Resolve.Memo.t

  module Resolved_select : sig
    type t =
      { src_fn : Filename.t Resolve.t
      ; dst_fn : Filename.t
      }
  end

  (** Resolved select forms *)
  val resolved_selects : t -> Resolved_select.t list Resolve.Memo.t

  (** Transitive closure of all used ppx rewriters *)
  val pps : t -> lib list Resolve.Memo.t

  (** Sub-systems used in this compilation context *)
  val sub_systems : t -> sub_system list Memo.t
end

(** {1 Library name resolution} *)

(** Collection of libraries organized by names *)
module DB : sig
  type lib := t

  (** A database allow to resolve library names *)
  type t = db

  val installed : Context.t -> t Memo.t

  module Resolve_result : sig
    type db := t
    type t

    val not_found : t
    val found : Lib_info.external_ -> t
    val to_dyn : t Dyn.builder
    val redirect_by_name : db -> Loc.t * Lib_name.t -> t
    val redirect_by_id : db -> Lib_id.t -> t
    val redirect_in_the_same_db : Loc.t * Lib_name.t -> t
  end

  (** Create a new library database. [resolve] is used to resolve library names
      in this database.

      When a library is not found, it is looked up in the parent database if
      any.

      [all] returns the list of names of libraries available in this database. *)
  val create
    :  parent:t option
    -> resolve:(Lib_name.t -> Resolve_result.t list Memo.t)
    -> resolve_lib_id:(Lib_id.t -> Resolve_result.t Memo.t)
    -> all:(unit -> Lib_name.t list Memo.t)
    -> instrument_with:Lib_name.t list
    -> unit
    -> t

  val find : t -> Lib_name.t -> lib option Memo.t
  val find_even_when_hidden : t -> Lib_name.t -> lib option Memo.t
  val find_lib_id : t -> Lib_id.t -> lib option Memo.t
  val find_lib_id_even_when_hidden : t -> Lib_id.t -> lib option Memo.t
  val available : t -> Lib_name.t -> bool Memo.t
  val available_by_lib_id : t -> Lib_id.t -> bool Memo.t

  (** Retrieve the compile information for the given library. Works for
      libraries that are optional and not available as well. *)
  val get_compile_info : t -> allow_overlaps:bool -> Lib_id.t -> (lib * Compile.t) Memo.t

  val resolve : t -> Loc.t * Lib_name.t -> lib Resolve.Memo.t

  (** Like [resolve], but will return [None] instead of an error if we are
      unable to find the library. *)
  val resolve_when_exists : t -> Loc.t * Lib_name.t -> lib Resolve.t option Memo.t

  (** Resolve libraries written by the user in a [dune] file. The resulting list
      of libraries is transitively closed and sorted by the order of
      dependencies.

      This function is for executables or melange.emit stanzas. *)
  val resolve_user_written_deps
    :  t
    -> [ `Exe of (Loc.t * string) Nonempty_list.t | `Melange_emit of string ]
    -> allow_overlaps:bool
    -> forbidden_libraries:(Loc.t * Lib_name.t) list
    -> Lib_dep.t list
    -> pps:(Loc.t * Lib_name.t) list
    -> dune_version:Dune_lang.Syntax.Version.t
    -> Compile.t

  val resolve_pps : t -> (Loc.t * Lib_name.t) list -> lib list Resolve.Memo.t

  (** Return the list of all libraries in this database. If [recursive] is true,
      also include libraries in parent databases recursively. *)
  val all : ?recursive:bool -> t -> Set.t Memo.t

  val instrumentation_backend
    :  t
    -> Loc.t * Lib_name.t
    -> Preprocess.Without_instrumentation.t option Resolve.Memo.t
end

(** {1 Transitive closure} *)

val closure : t list -> linking:bool -> t list Resolve.Memo.t

(** [descriptive_closure ~with_pps libs] computes the smallest set of libraries
    that contains the libraries in the list [libs], and that is transitively
    closed. If [with_pps = true], then the dependencies towards ppx-rewriters
    are also taken into account (note that runtime dependendies of ppx-rewriters
    are always taken into account, regardless of the value of the flag
    [with_pps]). The output list is guaranteed to have no duplicates and to be
    sorted. The difference with [closure libs] is that the latter may raise an
    error when overlapping implementations of virtual libraries are detected.
    [descriptive_closure libs] makes no such check. *)
val descriptive_closure : t list -> with_pps:bool -> t list Memo.t

(** {1 Sub-systems} *)

module Sub_system : sig
  type lib := t
  type t = sub_system = ..

  module type S = sig
    module Info : Sub_system_info.S

    type t
    type sub_system += T of t

    val instantiate
      :  resolve:(Loc.t * Lib_name.t -> lib Resolve.Memo.t)
      -> get:(loc:Loc.t -> lib -> t option Memo.t)
      -> lib
      -> Info.t
      -> t Memo.t

    val public_info : (t -> Info.t Resolve.Memo.t) option
  end

  module Register (M : S) : sig
    (** Get the instance of the subsystem for this library *)
    val get : lib -> M.t option Memo.t
  end
end

val to_dune_lib
  :  t
  -> modules:Modules.With_vlib.t
  -> foreign_objects:Path.t list
  -> melange_runtime_deps:Path.t list
  -> public_headers:Path.t list
  -> dir:Path.t
  -> Dune_package.Lib.t Resolve.Memo.t

(** Local libraries *)
module Local : sig
  type lib := t
  type t = private lib

  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
  val hash : t -> int
  val of_lib : lib -> t option
  val of_lib_exn : lib -> t
  val to_lib : t -> lib
  val info : t -> Lib_info.local
  val obj_dir : t -> Path.Build.t Obj_dir.t

  include Comparable_intf.S with type key := t
end

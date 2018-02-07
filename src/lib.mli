open Import

module Internal : sig
  type t = Path.t * Jbuild.Library.t
end

type t

val internal : Internal.t -> t
val external_ : Findlib.Package.t -> t

val to_either : t -> (Internal.t, Findlib.Package.t) either

val get_internal : t -> Internal.t option
val is_local : t -> bool

val src_dir : t -> Path.t option
val obj_dir : t -> Path.t

module Set : Set.S with type elt := t

val lib_obj_dir : Path.t -> Jbuild.Library.t -> Path.t

(*val deps : t -> string list*)

val include_paths : t list -> stdlib_dir:Path.t -> Path.Set.t
val include_flags : t list -> stdlib_dir:Path.t -> _ Arg_spec.t

val c_include_flags : t list -> stdlib_dir:Path.t -> _ Arg_spec.t

val link_flags : t list -> mode:Mode.t -> stdlib_dir:Path.t -> _ Arg_spec.t

(** All the library archive files (.a, .cmxa, _stubs.a, ...)  that
    should be linked in when linking an executable. *)
val archive_files : t list -> mode:Mode.t -> ext_lib:string -> Path.t list

val jsoo_runtime_files : t list -> Path.t list
val jsoo_archives : t -> Path.t list

(** [public_name] if present, [name] if not *)
val best_name : t -> string

val describe : t -> string

val remove_dups_preserve_order : t list -> t list

val ppx_runtime_libraries : t -> String_set.t
val requires : t -> Jbuild.Lib_deps.t
val scope : t -> [`Dir of Path.t | `External]

val public_name : t -> string option

type local =
  { src: Path.t
  ; name: string
  }

val local : t -> local option

val unique_id : t -> string

val exists_name : t -> f:(string -> bool) -> bool

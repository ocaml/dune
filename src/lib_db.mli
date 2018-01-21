(** Where libraries are

    This module is used to implement [Super_context.Libs].
*)

open Import

type t

val create
  :  Findlib.t
  -> scopes:Jbuild.Scope.t list
  -> (Path.t * Jbuild.Library.t) list
  -> t

val find     : t -> included:bool -> from:Path.t -> string -> Lib.t option
val find_exn : t -> included:bool -> from:Path.t -> string -> Lib.t

val resolve_exn : t -> from:Path.t -> string -> Lib.t
val best_name : Lib.t -> string

val internal_libs_without_non_installable_optional_ones : t -> Lib.Internal.t list

val interpret_lib_deps
  :  t
  -> dir:Path.t
  -> Jbuild.Lib_dep.t list
  -> (Lib.Internal.t * bool) list * (Findlib.package * bool) list * fail option

val best_lib_dep_names_exn
  :  t
  -> dir:Path.t
  -> Jbuild.Lib_dep.t list
  -> string list

type resolved_select =
  { src_fn : string
  ; dst_fn : string
  }

val resolve_selects
  :  t
  -> from:Path.t
  -> Jbuild.Lib_dep.t list
  -> resolved_select list

val lib_is_available : t -> from:Path.t -> string -> bool

(** For [Findlib.closure] *)
val local_public_libs : t -> Path.t String_map.t

(** Unique name, even for internal libraries *)
val unique_library_name : t -> Lib.t -> string

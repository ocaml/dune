(** Where libraries are *)

open Import

type t

val create : Findlib.t -> (Path.t * Jbuild_types.Stanza.t list) list -> t

val find : t -> string -> Lib.t

val find_internal : t -> string -> Lib.Internal.t option

val internal_libs_without_non_installable_optional_ones : t -> Lib.Internal.t list

val interpret_lib_deps
  :  t
  -> dir:Path.t
  -> Jbuild_types.Lib_dep.t list
  -> Lib.Internal.t list * Findlib.package list * fail option

type resolved_select =
  { src_fn : string
  ; dst_fn : string
  }

val resolve_selects
  :  t
  -> Jbuild_types.Lib_dep.t list
  -> resolved_select list

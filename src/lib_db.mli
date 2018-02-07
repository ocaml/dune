(** Where libraries are

    This module is used to implement [Super_context.Libs].
*)

open Import

type t

type resolved_select =
  { src_fn : string
  ; dst_fn : string
  }

module Scope : sig
  (** A scope can be used to resolve library names to libraries as they are
      defined in the build - external or internal.

      Every directory in the context's build tree corresponds to a particular
      scope which can be found with [find_scope]. The only exception to this is
      the external scope.
  *)

  type t

  val find : t With_required_by.t -> string -> Lib.t option
  val find_exn : t With_required_by.t -> string -> Lib.t

  val lib_is_available : t With_required_by.t -> string -> bool

  val root : t -> Path.t
  val name : t -> string

  val resolve : t With_required_by.t -> string -> (Package.t, string) result

  val required_in_jbuild : t -> jbuild_dir:Path.t -> t With_required_by.t

  val interpret_lib_deps
    :  t With_required_by.t
    -> Jbuild.Lib_dep.t list
    -> Lib.Internal.t list * Findlib.Package.t list * fail option

  val resolve_selects
    :  t With_required_by.t
    -> Jbuild.Lib_dep.t list
    -> resolved_select list

  val best_lib_dep_names_exn
    :  t With_required_by.t
    -> Jbuild.Lib_dep.t list
    -> string list

  (** [all_ppx_runtime_deps_exn t deps] takes the transitive closure of [deps]
      and return the set of all the ppx runtime dependencies of these
      libraries. *)
  val all_ppx_runtime_deps_exn
    :  t With_required_by.t
    -> Jbuild.Lib_dep.t list
    -> String_set.t
end

val create
  :  Findlib.t
  -> scopes:Jbuild.Scope.t list
  -> root:Path.t
  -> (Path.t * Jbuild.Library.t) list
  -> t

val internal_libs_without_non_installable_optional_ones : t -> Lib.Internal.t list

(** For [Findlib.closure] *)
val local_public_libs : t -> Path.t String_map.t

(** Unique name, even for internal libraries *)
val unique_library_name : t -> Lib.t -> string

val find_scope : t -> dir:Path.t -> Scope.t
val find_scope' : t -> dir:Path.t -> Scope.t With_required_by.t

(** Includes the private libraries not belonging to any named scope. Corresopnds
    to the context's build root path.*)
val anonymous_scope : t -> Scope.t

(** Contains only publicly, and external (findlib) libraries *)
val external_scope : t -> Scope.t

(** Find scope by the their explicit names (opam package names) [""] corresponds
    to the anonymous scope *)
val find_scope_by_name_exn : t -> name:string -> Scope.t

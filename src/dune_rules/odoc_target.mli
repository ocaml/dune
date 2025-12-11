(** Target types for odoc artifacts.

    This module defines the target types used to identify documentation artifacts.
    Targets represent either module documentation (for libraries) or page documentation
    (for packages and the toplevel index). *)

open Import

(** Documentation generation modes.

    - [Local_only]: Generate docs only for workspace packages (fast, @doc alias)
    - [Full]: Generate docs for workspace packages and all dependencies (@doc-full) *)
module Doc_mode : sig
  type t =
    | Local_only
    | Full

  val subdir : t -> string
  val html_subdir : t -> string
  val json_subdir : t -> string
  val all : t list
end

(** Page metadata for mld documentation files. *)
type page =
  { name : string (** Page name, may include hierarchy like "foo/bar" *)
  ; pkg_libs : Lib.t list (** Package's libraries, used as base deps for linking *)
  }

(** Module metadata for library documentation. *)
type mod_ =
  { visible : bool
    (** For local libs: from [Module.visibility].
            For installed libs: true unless name contains [__]. *)
  ; module_name : Module_name.t
  }

(** Target GADT representing documentation targets.

    The type parameter indicates whether this is a module target ([mod_]) or
    a page target ([page]).

    - [Lib]: A library within a package (pkg/libname structure)
    - [Private_lib]: A private library using unique_name for disambiguation
    - [Pkg]: Package-level pages (index.mld, etc.)
    - [Toplevel]: The root index page for all documentation *)
type _ t =
  | Lib : Package.Name.t * Lib.t -> mod_ t
  | Private_lib : string * Lib.t -> mod_ t
  | Pkg : Package.Name.t -> page t
  | Toplevel : Doc_mode.t -> page t

(** Existential wrapper for targets of any kind. *)
type any = Any : 'a t -> any

(** Compare two targets for sorting. Orders by target type then by name. *)
val compare_any : any -> any -> Ordering.t

(** Create a target for a library.

    Uses [Lib_info.package] for local libraries and [Package_discovery] for
    installed libraries. Private libraries without packages get [Private_lib]
    targets with unique names. *)
val target_of_lib : Package_discovery.t -> Lib.t -> mod_ t Memo.t

(** Artifact discovery for odoc packages and libraries.

    This module discovers all documentation artifacts (modules and pages) for
    packages and libraries in the workspace. It handles both local packages
    (in the workspace) and installed packages (from opam), and resolves
    odoc-config dependencies to find transitive documentation requirements. *)

open Import

(** {1 Package Discovery} *)

(** Get workspace packages, respecting the [-p] flag mask if present. *)
val get_workspace_packages : unit -> Package.Name.t list Memo.t

(** Check if a package is a local workspace package (vs installed). *)
val is_local_package : Package.Name.t -> bool Memo.t

(** Get all libraries in a package (local or installed). *)
val libs_of_pkg : Context.t -> pkg:Package.Name.t -> Lib.t list Memo.t

(** Get all private libraries (local libraries without a package) in the workspace.
    Returns empty list in [-p] mode since private libraries don't belong to packages. *)
val get_private_libraries : Context.t -> Lib.Local.t list Memo.t

(** {1 Toplevel Index} *)

(** Create the toplevel index artifact that lists all packages.
    This is the root entry point for the documentation. *)
val toplevel_index_artifact
  :  Context.t
  -> mode:Odoc_target.Doc_mode.t
  -> Odoc_artifact.t Memo.t

(** Items displayed in the toplevel index. *)
module Toplevel_index : sig
  type pkg_item =
    { name : string
    ; version : Package_version.t option
    }

  type private_lib_item =
    { unique_name : string
    ; display_name : string
    }

  type item =
    | Package of pkg_item
    | Private_lib of private_lib_item

  (** Get items to display in the toplevel index for the given mode. *)
  val get_items : mode:Odoc_target.Doc_mode.t -> Context.t -> item list Memo.t

  (** Generate mld content for the toplevel index. *)
  val mld_content : item list -> string
end

(** {1 Artifact Discovery} *)

(** Discover all artifacts for a package or private library.

    This is the main entry point for artifact discovery. Given a package name
    or private library unique name, discovers all module and page artifacts.

    Returns [(artifacts, lib_subdirs)] where [lib_subdirs] are the library
    subdirectory names within the package. *)
val discover_package_artifacts
  :  Super_context.t
  -> Context.t
  -> pkg_or_lib_unique_name:string
  -> (Odoc_artifact.t list * string list) Memo.t

(** Collect all visible .odocl files for sidebar/index generation.

    Returns [(workspace_pkgs, odocl_files)] containing the workspace package
    names and paths to all non-hidden .odocl files.

    For [Full] mode, includes all transitive dependencies (installed packages).
    For [Local_only] mode, only includes workspace packages. *)
val collect_all_visible_odocls
  :  Super_context.t
  -> mode:Odoc_target.Doc_mode.t
  -> unit
  -> (Package.Name.t list * Path.Build.t list) Memo.t

(** {1 Dependency Expansion} *)

(** Expand packages with their odoc-config dependencies transitively.

    Starting from the initial packages and private libraries, follows
    library dependencies and odoc-config [(documentation (depends ...))]
    declarations to find all packages needed for complete documentation.

    Uses [Lib.descriptive_closure] rather than [Lib.closure] because
    multiple implementations of virtual libraries may be present when
    documenting multiple packages together. *)
val expand_packages_with_odoc_config
  :  Context.t
  -> packages:Package.Name.t list
  -> private_libs:Lib.t list
  -> Package.Name.Set.t Memo.t

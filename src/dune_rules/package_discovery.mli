(** Package discovery for installed (opam) packages.

    This module discovers information about packages installed via opam,
    using opam's .changes files and installation metadata. It does NOT
    handle local workspace packages - use {!Dune_rules.Dune_load} or
    {!Dune_rules.Scope} for those. *)

open Import

(** The type representing package discovery state for installed packages *)
type t

(** Create a new package discovery instance from a dune context *)
val create : context:Context.t -> t Memo.t

(** Find which installed package a library belongs to.

    @raise Code_error if called on a local library. *)
val package_of_library : t -> Lib.t -> Package.Name.t option

(** Get all libraries belonging to an installed package.
    Returns an empty list for unknown/uninstalled packages. *)
val libraries_of_package : t -> Package.Name.t -> Lib.t list

(** Get all mld files belonging to an installed package.
    Returns an empty list for unknown/uninstalled packages. *)
val mlds_of_package : t -> Package.Name.t -> Path.t list

(** Get the source file (cmti/cmt) for a specific module in an installed library.
    Returns the path to the .cmti file if it exists, otherwise .cmt. *)
val module_source_file : t -> lib:Lib.t -> module_name:string -> Path.t option

(** Get the odoc configuration for an installed package. *)
val config_of_package : t -> Package.Name.t -> Odoc_config.t

(** Get the version of an installed package.
    Returns [None] if the package is not installed or version cannot be determined. *)
val version_of_package : t -> Package.Name.t -> string option Memo.t

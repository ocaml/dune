(** Scope and naming utilities for odoc v2 library disambiguation *)

open Import

(** Identifies a scope for documentation generation - either a package or a private library.

    This type provides validated scope identification. For private libraries,
    the project is resolved and validated during construction, ensuring that
    if you have a [Scope_id.t], it refers to a valid scope. *)
module Scope_id : sig
  type t =
    | Package of Package.Name.t
    | Private_lib of
        { unique_name : string
        ; lib_name : Lib_name.t
        ; project : Dune_project.t
        }

  (** Parse and validate a scope ID string.

      For strings containing '@' (private library format "libname@projectkey"),
      this resolves the project from the key and validates it exists.
      For other strings, treats them as package names.

      Returns a [Memo.t] because private library validation requires
      looking up the project by its key. *)
  val of_string : string -> t Memo.t

  (** Convert to string for use in paths. *)
  val to_string : t -> string

  (** Check if this is a private library (vs a regular package). *)
  val is_private_lib : t -> bool

  (** Get as a Package.Name.t. For private libs, this parses the unique_name
      as a package name (useful for certain operations that need a Package.Name.t). *)
  val as_package_name : t -> Package.Name.t
end

(** Scope key encoding for v2 library names.

    In odoc v2, private libraries from different projects need unique identifiers
    to avoid name collisions. The Scope_key module encodes project information
    into library names using the format "libname@key" where key is a 12-character
    hash derived from the project name and root path. *)
module Scope_key : sig
  (** Convert a library name to its v2 unique name format.

      For a private library in the given project, generates "libname@key" where
      key is a 12-character hash uniquely identifying the project. *)
  val to_string : Lib_name.t -> Dune_project.t -> string
end

(** Generate a unique name for a local library.

    - Public libraries: use their plain name
    - Private libraries: use v2 format "libname@key" from Scope_key.to_string
    - Raises: assertion failure if called on installed libraries *)
val lib_unique_name : Lib.Local.t -> string

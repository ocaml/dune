open Import

(** A vendor stanza specifies a vendored subdirectory and which
    libraries/packages to expose from it.

    Syntax:
    {[
      (vendor fmt.0.9.0 (libraries fmt fmt.tty))
      (vendor yojson.1.7.0 (libraries (yojson :as yojson_v1)))
      (vendor cohttp.6.0.0 (package cohttp) (libraries cohttp cohttp-lwt))
    ]}

    The [(libraries ...)] field uses ordered set language with aliasing:
    - [:standard] means all libraries found in the directory
    - [\ name] excludes items from the set
    - [(name :as alias)] exposes library under a different name

    The [(package <name>)] field optionally restricts this stanza to a
    specific package. Multiple vendor stanzas for the same directory can
    each specify a different package. *)

module Library_entry : sig
  type t =
    { lib_name : Lib_name.t
    ; alias : Lib_name.t option
    }

  val to_dyn : t -> Dyn.t
  val exposed_name : t -> Lib_name.t
  val has_alias : t -> bool
end

module Libraries_ast : sig
  type t

  val decode : t Decoder.t
  val standard : t
  val is_standard : t -> bool
  val has_aliasing : t -> bool

  (** Evaluate the AST against a standard set of libraries.
      Returns list of (original_name, exposed_name) pairs. *)
  val eval : t -> standard:Lib_name.t list -> (Lib_name.t * Lib_name.t) list
end

type t =
  { loc : Loc.t
  ; directory : Filename.t
  ; libraries : Libraries_ast.t
  ; package : Package_name.t option
  }

val decode : t Decoder.t
val to_dyn : t -> Dyn.t

(** Evaluate the libraries specification against a standard set of discovered libraries.
    Returns list of (original_name, exposed_name) pairs where exposed_name may differ
    if aliasing is used. *)
val eval_libraries : t -> standard:Lib_name.t list -> (Lib_name.t * Lib_name.t) list

(** Returns true if libraries is just :standard (needs scanning) *)
val is_standard_libraries : t -> bool

(** Get the package this stanza applies to, if specified *)
val package : t -> Package_name.t option

(** Check if this vendor stanza applies to the given package.
    If no package is specified in the stanza, it applies to all packages.
    If a package is specified, it only applies to that package. *)
val applies_to_package : t -> pkg_name:Package_name.t -> bool

(** Check if any vendor stanza in the list applies to the given package. *)
val applies_to_package_list : t list -> pkg_name:Package_name.t -> bool

(** Find the vendor stanza that applies to a library based on its package,
    and return the library's visibility and alias information.
    [lib_pkg] should be the package name of the library (if it has one).
    Returns:
    - [`Excluded] if no stanza applies to the library's package
    - [`Included None] if the library is visible under its original name
    - [`Included (Some alias)] if the library is visible under an alias *)
val find_library_status
  :  t list
  -> lib_name:Lib_name.t
  -> lib_pkg:Package_name.t option
  -> [ `Excluded | `Included of Lib_name.t option ]

(** Check if a package is visible according to vendor stanzas.
    Returns:
    - [true] if no vendor stanzas (no filtering)
    - [true] if a stanza explicitly specifies this package
    - [true] if a stanza has no package field and no aliasing (expose all)
    - [false] if a stanza has aliasing but no explicit package (aliased libs only)
    - [false] if stanzas exist but none match this package *)
val is_package_visible : t list -> pkg_name:Package_name.t -> bool

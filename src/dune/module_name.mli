open Stdune

(** Represents a valid OCaml module name *)
type t

include Stringlike_intf.S with type t := t

val add_suffix : t -> string -> t

val compare : t -> t -> Ordering.t

val uncapitalize : t -> string

val pp_quote : Format.formatter -> t -> unit

module Per_item : Per_item_intf.S with type key = t

module Infix : Comparator.OPS with type t = t

val of_local_lib_name : Lib_name.Local.t -> t

val to_local_lib_name : t -> Lib_name.Local.t

module Unique : sig
  type name

  (** We use [Unique] module names for OCaml unit names. These must be unique
      across all libraries within a given linkage, so these names often involve
      mangling on top of the user-written names because the user-written names
      are only unique within a library.

      These are the names that are used for the .cmi and .cmx artifacts.

      Since [Unique] module names are sometimes mangled, they should not appear
      in any user-facing messages or configuration files. *)
  type nonrec t

  val of_string : string -> t

  val of_name_assuming_needs_no_mangling : name -> t

  (** We allow invalid module names for backwards compatibility *)
  val of_path_assuming_needs_no_mangling_allow_invalid : Path.t -> t

  val to_dyn : t -> Dyn.t

  val to_name : t -> loc:Loc.t -> name

  val compare : t -> t -> Ordering.t

  val artifact_filename : t -> ext:string -> string

  include Dune_lang.Conv.S with type t := t

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t
end
with type name := t

val wrap : t -> with_:t -> Unique.t

module Map : Map.S with type key = t

module Set : sig
  include Set.S with type elt = t

  val to_dyn : t -> Dyn.t
end

val of_string_allow_invalid : Loc.t * string -> t

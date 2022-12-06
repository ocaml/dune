open Import

(** Represents a valid OCaml module name *)
type t

(** Description of valid module names *)
val valid_format_doc : User_message.Style.t Pp.t

include Stringlike_intf.S with type t := t

val add_suffix : t -> string -> t

val equal : t -> t -> bool

val compare : t -> t -> Ordering.t

val uncapitalize : t -> string

val pp_quote : Format.formatter -> t -> unit

module Per_item : sig
  include Per_item_intf.S with type key = t

  val decode : default:'a -> 'a Dune_lang.Decoder.t -> 'a t Dune_lang.Decoder.t
end

module Infix : Comparator.OPS with type t = t

val of_local_lib_name : Loc.t * Lib_name.Local.t -> t

val to_local_lib_name : t -> Lib_name.Local.t

module Unique : sig
  type name := t

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

  val to_string : t -> string

  val compare : t -> t -> Ordering.t

  val equal : t -> t -> bool

  val artifact_filename : t -> ext:string -> string

  include Dune_lang.Conv.S with type t := t

  include Comparable_intf.S with type key := t
end

module Path : sig
  type nonrec t = t list

  val compare : t -> t -> Ordering.t

  val to_dyn : t -> Dyn.t

  val to_string : t -> string

  val uncapitalize : t -> string

  module Map : Stdune.Map.S with type key = t

  module Set : Stdune.Set.S with type elt = t and type 'a map = 'a Map.t

  val wrap : t -> Unique.t

  val encode : t -> Dune_lang.t list

  val decode : t Dune_lang.Decoder.t

  val append_double_underscore : t -> t
end

val wrap : t -> with_:Path.t -> Unique.t

include Comparable_intf.S with type key := t

module Map_traversals : sig
  val parallel_iter : 'a Map.t -> f:(t -> 'a -> unit Memo.t) -> unit Memo.t

  val parallel_map : 'a Map.t -> f:(t -> 'a -> 'b Memo.t) -> 'b Map.t Memo.t
end

val of_string_allow_invalid : Loc.t * string -> t

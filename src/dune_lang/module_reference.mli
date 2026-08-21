open Import

(** A reference to a module in a dune file.

    Starting with version 3.25 of the dune language, references may contain
    multiple components, for example [Foo.Bar]. *)
type t

val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val loc : t -> Loc.t
val path : t -> Module_name.Path.t
val to_string : t -> string
val to_dyn : t -> Dyn.t
val of_string : Syntax.Version.t -> Loc.t * string -> t
val is_qualified : t -> bool
val is_legacy : t -> bool
val decode : t Decoder.t

module Per_item : sig
  include Per_item with type key = t

  val decode : default:'a -> 'a Decoder.t -> 'a t Decoder.t
  val repr : 'a Repr.t -> 'a t Repr.t

  (** Find the value associated with a module. [path] is its logical path and
      [name] is its final component. The latter preserves the lookup semantics
      of dune language versions before 3.25. *)
  val find : 'a t -> path:Module_name.Path.t -> name:Module_name.t -> 'a

  val explicit_references : 'a t -> key list
end

open Import

(** A reference to a module in a dune file.

    Starting with version 3.25 of the dune language, references may contain
    multiple components, for example [Foo.Bar]. *)
type t

val loc : t -> Loc.t
val path : t -> Module_name.Path.t
val to_string : t -> string
val of_string : Syntax.Version.t -> Loc.t * string -> t
val is_qualified : t -> bool

(** Reject a qualified reference unless subdirectories are included with
    qualified names. *)
val validate_qualified : t -> include_subdirs:Include_subdirs.t -> unit

module Per_item : sig
  type key = t
  type 'a t

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val for_all : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val fold : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc) -> 'acc
  val exists : 'a t -> f:('a -> bool) -> bool

  module Make_monad_traversals (Monad : sig
      include Monad.S

      val all : 'a t list -> 'a list t
    end) : sig
    val fold : 'a t -> init:'acc -> f:('a -> 'acc -> 'acc Monad.t) -> 'acc Monad.t
    val map : 'a t -> f:('a -> 'b Monad.t) -> 'b t Monad.t
  end

  val decode : default:'a -> 'a Decoder.t -> 'a t Decoder.t
  val repr : 'a Repr.t -> 'a t Repr.t

  (** Find the value associated with a module by its logical path. In dune
      language versions before 3.25, only the final component is used. *)
  val find : 'a t -> Module_name.Path.t -> 'a

  val explicit_references : 'a t -> key list
end

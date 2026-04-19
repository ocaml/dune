open Stdune

val action : patch:Path.t -> Dune_engine.Action.t

module For_tests : sig
  val prefix_of_patch : loc:Loc.t -> string -> int
  val parse_patches : loc:Loc.t -> string -> Patch.t list
  val apply_patches : dir:Path.t -> Patch.t list -> unit
  val exec : loc:Loc.t -> dir:Path.t -> patch:Path.t -> unit Fiber.t
end

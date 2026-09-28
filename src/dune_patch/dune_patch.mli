open Stdune

val action : patch:Path.t -> Dune_engine.Action.t

module File_patch : sig
  type t

  val parse : loc:Loc.t -> patch_file:Path.t -> string -> t list
  val source : t -> Path.Local.t option
  val target : t -> Path.Local.t option
  val removes_source : t -> bool
  val preserves_source_mode : t -> bool
  val apply : t -> string option -> string option
  val to_dyn : t -> Dyn.t
end

module For_tests : sig
  val prefix_of_patch : patch_loc:Loc.t -> string -> int
  val parse_patches : loc:Loc.t -> patch_file:Path.t -> string -> Patch.t list
  val apply_patches : dir:Path.t -> Patch.t list -> unit
  val exec : loc:Loc.t -> dir:Path.t -> patch:Path.t -> unit Fiber.t
end

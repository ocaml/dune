open Import

val exec
  :  Loc.t
  -> patch_back:Path.t option
  -> (Path.t, Path.Build.t) Stdune.Action_types.Diff.t
  -> unit Fiber.t

(** Execute the comparison without registering any resulting promotion. *)
val exec_without_promotion
  :  Loc.t
  -> (Path.t, Path.Build.t) Stdune.Action_types.Diff.t
  -> unit Fiber.t

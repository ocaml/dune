(** Creation and management of sandboxes *)

open Stdune

type t

val dir : t -> Path.Build.t

(** [map_path t p] returns the path corresponding to [p] inside the sandbox. *)
val map_path : t -> Path.Build.t -> Path.Build.t

(** Create a new sandbox and copy or link dependencies inside it. *)
val create :
     mode:Sandbox_mode.some
  -> patch_back_source_tree:bool
  -> rule_loc:Loc.t
  -> deps:Dep.Facts.t
  -> rule_dir:Path.Build.t
  -> chdirs:Path.Set.t
  -> rule_digest:Digest.t
  -> expand_aliases:bool
  -> t

(** Move the targets created by the action from the sandbox to the build
    directory. Returns the set of files discovered in directory targets. *)
val move_targets_to_build_dir :
  t -> loc:Loc.t -> targets:Targets.t -> Path.Build.Set.t

val destroy : t -> unit

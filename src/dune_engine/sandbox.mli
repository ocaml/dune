(** Creation and management of sandboxes *)

open Import

type t

val dir : t -> Path.Build.t

(** [map_path t p] returns the path corresponding to [p] inside the sandbox. *)
val map_path : t -> Path.Build.t -> Path.Build.t

(** Create a new sandbox and copy or link dependencies inside it. *)
val create :
     mode:Sandbox_mode.some
  -> dune_stats:Dune_stats.t option
  -> rule_loc:Loc.t
  -> deps:Dep.Facts.t
  -> rule_dir:Path.Build.t
  -> rule_digest:Digest.t
  -> expand_aliases:bool
  -> t

(** Move all targets created by the action from the sandbox to the build
    directory, skipping the files for which [should_be_skipped] returns [true].

    Expands [targets] with the set of files discovered in directory targets. *)
val move_targets_to_build_dir :
     t
  -> loc:Loc.t
  -> should_be_skipped:(Path.Build.t -> bool)
  -> targets:Targets.Validated.t
  -> unit Targets.Produced.t

val destroy : t -> unit

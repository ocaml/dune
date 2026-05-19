(** Creation and management of sandboxes *)

open Import

type t

(** [is_sandboxed t] is [true] when [t] represents a real sandbox. *)
val is_sandboxed : t -> bool

(** [map_path t p] returns the path corresponding to [p] inside the sandbox. *)
val map_path : t -> Path.Build.t -> Path.Build.t

(** Delete targets left behind by interrupted non-sandboxed actions. *)
val cleanup_pending_targets : unit -> unit

(** Run [f] with a sandboxing context.

    [mode = None] is represented as a sandboxing context that maps paths to
    themselves and only tracks pending targets for cleanup. It is not affected
    by the live-sandbox throttle. *)
val with_
  :  mode:Sandbox_mode.some option
  -> Corrections.t
  -> rule_loc:Loc.t
  -> dirs:Path.Build.Set.t
  -> deps:Path.Set.t
  -> rule_dir:Path.Build.t
  -> rule_digest:Digest.t
  -> targets:Targets.Validated.t
  -> f:(t -> 'a Fiber.t)
  -> 'a Fiber.t

(** Move all targets created by the action from the sandbox to the build
    directory, skipping the files for which [should_be_skipped] returns [true].

    Expands [targets] with the set of files discovered in directory targets. *)
val move_targets_to_build_dir
  :  t
  -> should_be_skipped:(Path.Build.t -> bool)
  -> targets:Targets.Validated.t
  -> unit Fiber.t

(** Promoting rule targets to the source tree.

    See [Diff_promotion] for the logic related to promoting [diff]s. *)

open Import

val promote :
     dir:Path.Build.t
  -> targets:Digest.t Targets.Produced.t
  -> promote:Rule.Promote.t
  -> promote_source:
       (   chmod:(int -> int)
        -> delete_dst_if_it_is_a_directory:bool
        -> src:Path.Build.t
        -> dst:Path.Source.t
        -> unit Fiber.t)
  -> unit Fiber.t

(** The set of files created in the source tree that need to be deleted. *)
val files_in_source_tree_to_delete : unit -> Path.Source.Set.t

val delete_stale_dot_merlin_file :
     dir:Path.Build.t
  -> source_files_to_ignore:Path.Source.Set.t
  -> Path.Source.Set.t

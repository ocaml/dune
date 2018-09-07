open! Stdune

(** Represents saved and current partition digests
    and saved and current dependency graphs. *)
type t

(** Represents a set of targets that are logically related and are
    more likely to have internal changes than external (dependencies). *)
module Partition : sig
  type t

  (** Assigns path to a partition. *)
  val for_path : Path.t -> file_tree:File_tree.t -> t

  (** Assigns environmental variable dep in a certain context to a partition. *)
  val for_env_var : var:string -> context:string -> t
end

(** Loads .partition-db file from disk, or creates an empty db if it's absent. *)
val load : unit -> t

(** Clears all saved and current partition information. *)
val reset : t -> unit

(** Returns current digest of a partition, computing it if not yet computed.

    Digest is computed against dependencies that were saved
    from the previous build, so in the case any dependencies change,
    the truly correct digest will be only calculated during the next build,
    when new dependencies are saved. This shouldn't be a correctness
    issue in practice. *)
val get_current_digest
  : t
  -> Partition.t
  -> projects:Dune_project.t Dune_project.Name.Table.t
  -> file_tree:File_tree.t
  -> contexts:Context.t String.Map.t
  -> Digest.t

(** Checks if current digest of a partition is the same as saved digest, if any.

    Current digest will be computed eagerly even if saved digest is not present. *)
val is_clean
  :  t
  -> Partition.t
  -> projects:Dune_project.t Dune_project.Name.Table.t
  -> file_tree:File_tree.t
  -> contexts:Context.t String.Map.t
  -> bool

(** Updates the current partition dependency graph. *)
val register_dependency
  :  t
  -> target:Partition.t
  -> dependency:Partition.t
  -> unit

(** Dumps current digests and dependencies to disk. *)
val finalize : t -> unit

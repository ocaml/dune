(** This module is responsible for handling [diff]-related file promotions.

    See [Target_promotion] for the logic that handles promotion of rule targets. *)

open Import

module Annot : sig
  type t =
    { in_source : Path.Source.t
    ; in_build : Path.Build.t
    }

  val annot : t User_message.Annots.Key.t
end

module File : sig
  type t

  val source : t -> Path.Source.t
  val correction_file : t -> Path.t
  val to_dyn : t -> Dyn.t
  val in_staging_area : Path.Source.t -> Path.Build.t

  (** Register an intermediate file to promote. The build path may point to the
      sandbox and the file will be moved to the staging area. *)
  val register_intermediate
    :  source_file:Path.Source.t
    -> correction_file:Path.Build.t
    -> unit

  (** Register file to promote where the correction file is a dependency of the
      current action (rather than an intermediate file). [correction_file]
      refers to a path in the build dir, not in the sandbox (it can point to the
      sandbox, but the sandbox root will be stripped). *)
  val register_dep : source_file:Path.Source.t -> correction_file:Path.Build.t -> unit
end

type db

(** Promote all registered files if [!Clflags.auto_promote]. Otherwise dump the
    list of registered files to [_build/.to-promote]. *)
val finalize : unit -> unit

val load_db : unit -> db

type all =
  { present : File.t list
  ; missing : Path.Source.t list
  }

val partition_db : db -> Files_to_promote.t -> all
val promote_files_registered_in_last_run : Files_to_promote.t -> Path.Source.t list

(** [display_diffs db files] will only print the diffs of files that are both
    in [files] and in [db]. *)
val display_diffs : all -> unit Fiber.t

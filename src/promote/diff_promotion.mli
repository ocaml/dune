open Import

module File : sig
  type t

  val to_dyn : t -> Dyn.t
  val in_staging_area : Path.Source.t -> Path.Build.t
  val compare : t -> t -> ordering
  val source : t -> Path.Source.t
  val correction_file : t -> Path.t

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

(** Promote all registered files if [!Clflags.auto_promote]. Otherwise dump the
    list of registered files to [_build/.to-promote]. *)
val finalize : unit -> unit

val load_db : unit -> File.t list

val promote_files_registered_in_last_run
  :  Dune_rpc_private.Files_to_promote.t
  -> Path.Source.t list

(** [missing ~db files] returns the list of files in [files] but not in [db]. *)
val missing
  :  db:File.t list
  -> Dune_rpc_private.Files_to_promote.t
  -> Path.Source.t list Fiber.t

(** [display_diffs ~db files] will only print the diffs of files that are both
    in [files] and in [db]. *)
val display_diffs : db:File.t list -> Dune_rpc_private.Files_to_promote.t -> unit Fiber.t

(** [display_file_names ~db files] will only print the filenames of files that are
    both in [files] and in [db]. *)
val display_files : db:File.t list -> Dune_rpc_private.Files_to_promote.t -> unit Fiber.t

(** [display_corrected_contents ~db files] will print the changes in plain text
    of files that are both in [files] and in [db]. *)
val display_corrected_contents
  :  db:File.t list
  -> Dune_rpc_private.Files_to_promote.t
  -> unit

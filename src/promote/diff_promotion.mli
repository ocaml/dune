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
val filter_db : Dune_rpc_private.Files_to_promote.t -> File.t list -> File.t list
val diff_for_file : File.t -> (Print_diff.Diff.t, User_message.t) result Fiber.t
val promote_files_registered_in_last_run : Dune_rpc_private.Files_to_promote.t -> unit
val display : Dune_rpc_private.Files_to_promote.t -> unit Fiber.t

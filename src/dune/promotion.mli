open! Stdune

module File : sig
  type t

  val to_dyn : t -> Dyn.t

  (** Register an intermediate file to promote. The build path may point to the
      sandbox and the file will be moved to the staging area. *)
  val register_intermediate :
    source_file:Path.Source.t -> correction_file:Path.Build.t -> unit

  (** Register file to promote where the correction file is a dependency of the
      current action (rather than an intermediate file). [correction_file]
      refers to a path in the build dir, not in the sandbox (it can point to the
      sandbox, but the sandbox root will be stripped). *)
  val register_dep :
    source_file:Path.Source.t -> correction_file:Path.Build.t -> unit
end

(** Promote all registered files if [!Clflags.auto_promote]. Otherwise dump the
    list of registered files to [_build/.to-promote]. *)
val finalize : unit -> unit

(** Describe what files should be promoted. The second argument of [These] is a
    function that is called on files that cannot be promoted. *)
type files_to_promote =
  | All
  | These of Path.Source.t list * (Path.Source.t -> unit)

val promote_files_registered_in_last_run : files_to_promote -> unit

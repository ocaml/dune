open Import

(** Merlin identifiers allow the unique identification of a merlin file attached
    to a specific [library] or [executable] stanza. *)
type t

val for_lib : Lib_name.t -> t
val for_exe_target : Exe_target.t -> t

(** Merlin config folder name *)
val merlin_folder_name : Filename.t

(** Return the path of the merlin file for a given stanza *)
val merlin_file_path : Path.Build.t -> t -> Path.Build.t

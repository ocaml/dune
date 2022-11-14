open! Import

(** Merlin identifiers allow the unique identification of a merlin file attached
    to a specific [library] or [executable] stanza. *)
type t

val for_lib : Lib_name.t -> t

val for_exes : names:string list -> t

val for_melange : target:string -> t

(** Merlin config folder name *)
val merlin_folder_name : string

(** Return the path of the merlin file for a given stanza *)
val merlin_file_path : Path.Build.t -> t -> Path.Build.t

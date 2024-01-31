(** Loads dune files from the file system.

    Also responsible for evaluating dune files written in OCaml syntax. *)

open Import

type t

val dune_files : t -> context:Context_name.t -> Dune_file.t list Memo.t
val packages : t -> Package.t Package.Name.Map.t
val projects : t -> Dune_project.t list
val projects_by_root : t -> Dune_project.t Path.Source.Map.t

(** Load all dune files. This function is memoized. *)
val load : unit -> t Memo.t

val find_project : dir:Path.Build.t -> Dune_project.t Memo.t

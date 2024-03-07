(** Loads dune files from the workspace and query the workspace for various
    global data such as dune files, projects, pcakages.

    All the functions here are memoized. *)

open Import

val dune_files : Context_name.t -> Dune_file.t list Memo.t
val projects_by_root : unit -> Dune_project.t Path.Source.Map.t Memo.t
val find_project : dir:Path.Build.t -> Dune_project.t Memo.t
val stanzas_in_dir : Path.Build.t -> Dune_file.t option Memo.t
val mask : unit -> Only_packages.t Memo.t
val packages : unit -> Package.t Package.Name.Map.t Memo.t
val projects : unit -> Dune_project.t list Memo.t

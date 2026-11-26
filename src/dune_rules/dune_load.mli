(** Loads dune files from the workspace and query the workspace for various
    global data such as dune files, projects, pcakages.

    All the functions here are memoized. *)

open Import

type t =
  { dune_files : (Source_tree.Dir.t * Source.Dune_file.t) Appendable_list.t
  ; packages : Package.t Package.Name.Map.t
  ; projects : (Dune_project.t * Source_tree.Dir.t) list
  ; mask : Only_packages.t
  }

val load : unit -> t Memo.t

(** Evaluate package availability without constructing a build context. *)
val package_enabled : Package.t -> bool Memo.t

val mask : unit -> Only_packages.t Memo.t
val packages : unit -> Package.t Package.Name.Map.t Memo.t
val projects : unit -> Dune_project.t list Memo.t

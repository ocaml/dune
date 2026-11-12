(** Loads dune files from the workspace and query the workspace for various
    global data such as dune files, projects, pcakages.

    All the functions here are memoized. *)

open Import

val dune_files : Context_name.t -> Dune_file.t list Memo.t

(** Static stanzas owned by a package, including deprecated library names,
    in traversal order. *)
val stanzas_of_package
  :  Context_name.t
  -> Package.Name.t
  -> (Dune_file.t * Stanza.t) list Memo.t

val projects_by_output_root : Context_name.t -> Loaded_project.t Path.Build.Map.t Memo.t
val package_errors : Context_name.t -> User_message.t list Package.Name.Map.t Memo.t
val check_package : Context_name.t -> Package.Name.t -> unit Memo.t

(** The complete input tree enclosing [dir], without loading generated stanzas.
    This root may enclose several projects. *)
val source_tree_root : dir:Path.Build.t -> Source_tree.Rules.Dir.t option Memo.t

val source_tree_dir : dir:Path.Build.t -> Source_tree.Rules.Dir.t option Memo.t

val find_loaded_project_by_identity
  :  context:Context_name.t
  -> identity:Loaded_project.Identity.t
  -> Loaded_project.t Memo.t

val vendored_warnings : dir:Path.Build.t -> bool Memo.t
val find_loaded_project : dir:Path.Build.t -> Loaded_project.t Memo.t
val find_project : dir:Path.Build.t -> Dune_project.t Memo.t
val stanzas_in_dir : Path.Build.t -> Dune_file.t option Memo.t
val mask : unit -> Only_packages.t Memo.t
val mask_for_dir : dir:Path.Build.t -> [ `All | `Set of Package.Name.Set.t ] Memo.t
val packages : unit -> Package.t Package.Name.Map.t Memo.t
val packages_for_context : Context_name.t -> Package.t Package.Name.Map.t Memo.t

(** Shared workspace dependency closure for package search narrowing. *)
val package_closure : Package.Name.Set.t -> Package.Name.Set.t Memo.t

val package_closure_for_context
  :  Context_name.t
  -> Package.Name.Set.t
  -> Package.Name.Set.t Memo.t

val projects : unit -> Dune_project.t list Memo.t

open Stdune

val gen_rules : Super_context.t -> dir:Path.Build.t -> Build_system.Subdir_set.t

(** Generate rules for [.dune-package] and [META.<package-name>] files in a
    given super context and directory. Surprisingly, the current implementation
    generates rules for {b all} directories of the project, which are filtered
    out later in the [build_system.ml]. This function is called multiple times
    for the same project (with different [dir]), so we memoize it to avoid
    duplicating work. *)
val meta_and_dune_package_rules : Super_context.t -> dir:Path.Build.t -> unit

(* TODO: A seemingly more sensible approach for [meta_and_dune_package_rules] is
   to only generate rules for the given directory, or stop taking [dir]
   altogether and take [project] instead.

   aalekseyev: actually I think we should just remove
   [meta_and_dune_package_rules] from the interface and have [gen_rules] do
   everything. *)

val packages : Super_context.t -> Package.Name.Set.t Path.Build.Map.t

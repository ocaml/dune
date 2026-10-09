(** The dependency graph of the local packages of a lockdir together with all
    their transitive dependencies. Nodes are packages; edges are the "depends
    on" relation. The graph may contain cycles (only between local packages; the
    lockdir itself is acyclic). *)
type t

val create : Package_universe.t -> t

(** The local packages, i.e. the roots of the forward dependency graph. *)
val roots : t -> Package_name.t list

(** Immediate dependencies of a package (forward edges). *)
val dependencies : t -> Package_name.t -> Package_name.t list

(** Immediate dependents of a package: the packages that directly depend on it
    (reverse edges). Empty for a package that nothing depends on. *)
val dependents : t -> Package_name.t -> Package_name.t list

(** The [name.version] of a package in the graph. *)
val opam_package : t -> Package_name.t -> OpamPackage.t

val to_dyn : t -> Dyn.t

(** Render the forward dependency tree: one entry per local package (root). A
    package's subtree is expanded only the first time it is encountered; later
    encounters show just the package with its occurrence count as "(*N)", and
    an edge closing a dependency cycle is marked "(cycle)". No count is shown
    for packages involved in a cycle, where the count is not well-defined. This
    is the view rendered by [dune describe pkg tree]. *)
val pp_deps_tree : t -> _ Pp.t

(** Render the reverse ("why is this package installed?") tree rooted at
    [package]: its children are the packages that directly depend on it,
    recursively up to the local roots. Deduplicated and cycle-closing entries
    are marked as in [pp_deps_tree]. [package] must be present in the graph. *)
val pp_why : t -> Package_name.t -> _ Pp.t

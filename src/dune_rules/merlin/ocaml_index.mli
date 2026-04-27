open Import

(** This module provides support for the [ocaml-index] indexing tool. Its role
    is to index every value in the project by their definition in order for
    language servers to be able to fetch project-wide occurrences.

    Indexing all definition usages is a two step process:

    1. For all compilation contexts we generate the indexes for the modules in
       that cctx in the corresponding obj_dir.

    2. We aggregate all these separate indexes into a unique one. *)

(** [index_modules_rule ~modules_to_index cctx] sets up the rules needed to
    generate the indexes for the given modules and aggregates them in a
    cctx.ocaml-index file covering the whole compilation context.

    The callers decide which modules to index:
    - For libraries/melange: all user-written modules.
    - For executables: modules reachable from each top level module. *)
val index_modules_rule
  :  modules_to_index:Module.t list Action_builder.t
  -> Compilation_context.t
  -> unit Memo.t

(** [context_indexes] lists all the available cctx.ocaml-index files in the
    given context. *)
val context_indexes : Context.t -> Path.t list Action_builder.t

(** [project_rule] adds a rule that will aggregate all the generated indexes
    into one global, project-wide, index. *)
val project_rule : Super_context.t -> Dune_project.t -> unit Memo.t

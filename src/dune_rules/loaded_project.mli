open Import

type t

val create
  :  project:Dune_project.t
  -> owner:Package.Name.t option
  -> source_tree_root:Source_tree.Dir.t
  -> build_dir:Path.Build.t
  -> partition_root:Path.Build.t
  -> stanza_mask:Package.Name.Set.t option
  -> t

val project : t -> Dune_project.t

(** The project's root relative to its context directory. It names the
    project independently of the context. *)
val source_root : t -> Path.Local.t

val source_tree_dir : t -> Path.Build.t -> Source_tree.Dir.t option Memo.t

(** The build directory of the project's context. *)
val build_dir : t -> Path.Build.t

val partition_root : t -> Path.Build.t
val output_root : t -> Path.Build.t
val visible_project : t -> Dune_project.t

(** Build selection is separate from admitted package definitions: for example,
    disabled packages still need their opam metadata generated. *)
val stanza_mask : t -> Package.Name.Set.t option

(** Dependency warning defaults, independent of authored directory status and
    package admission. Authored vendored project roots also enable this policy. *)
val vendored_warnings : t -> bool

(** The mounted package whose resolver this project uses, if any. Projects
    vendored inside a mount use their mount's. *)
val owner : t -> Package.Name.t option

val packages : t -> Package.t Package.Name.Map.t
val output_path : t -> Path.Local.t -> Path.Build.t

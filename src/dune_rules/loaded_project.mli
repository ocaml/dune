open Import

module Identity : sig
  type t

  val workspace : Path.Source.t -> t
  val mounted : package:Package.Name.t -> project_root:Path.Local.t -> t
  val equal : t -> t -> bool
  val repr : t Repr.t
  val digest : t -> Dune_digest.t
  val to_dyn : t -> Dyn.t
end

type t

val create
  :  project:Dune_project.t
  -> identity:Identity.t
  -> source_root:Source_path.t
  -> source_tree_root:Source_tree.Rules.Dir.t
  -> partition:Build_partition.t
  -> output_root:Path.Build.t
  -> visible_packages:Package.Name.Set.t option
  -> stanza_mask:Package.Name.Set.t option
  -> vendored_warnings:bool
  -> t

val project : t -> Dune_project.t
val identity : t -> Identity.t
val source_root : t -> Source_path.t
val source_tree_root : t -> Source_tree.Rules.Dir.t
val source_tree_dir : t -> Path.Build.t -> Source_tree.Rules.Dir.t option Memo.t
val partition : t -> Build_partition.t
val output_root : t -> Path.Build.t
val visible_packages : t -> Package.Name.Set.t option

(** Build selection is separate from admitted package definitions: for example,
    disabled packages still need their opam metadata generated. *)
val stanza_mask : t -> Package.Name.Set.t option

(** Dependency warning defaults, independent of authored directory status and
    package admission. Authored vendored project roots also enable this policy. *)
val vendored_warnings : t -> bool

val hide_packages : t -> t

(** The selected package whose resolver this project uses, if any. Authored
    vendored subprojects retain their ordinary package scopes instead. *)
val owner : t -> Package.Name.t option

val packages : t -> Package.t Package.Name.Map.t
val output_path : t -> Source_path.t -> Path.Build.t option
val source_path : t -> Path.Build.t -> Source_path.t option
val to_dyn : t -> Dyn.t

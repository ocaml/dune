open Import

val root_dir_basename : string
val artifact_dir_basename : string
val package_of_artifact_path : Path.Build.t -> Package.Name.t option

module Candidate : sig
  type t

  val name : t -> Package.Name.t
  val lock_pkg : t -> Dune_pkg.Lock_dir.Pkg.t
  val source_root : t -> Path.Build.t option
  val files_dir : t -> Path.Build.t
  val artifact_root : t -> Path.Build.t
  val dependencies : t -> Package.Name.Set.t Memo.t
  val paths : t -> Path.Build.t Opam_build.Paths.t
end

val candidates : Context_name.t -> Candidate.t list Memo.t
val find_candidate : Context_name.t -> Package.Name.t -> Candidate.t option Memo.t
val source : Candidate.t -> Opam_build.Source_input.t Memo.t

module Mounted : sig
  type t

  val candidate : t -> Candidate.t
  val tree : t -> Source_tree.Rules.Mounted.t option
  val projects : t -> (Dune_project.t * Source_tree.Rules.Dir.t) list
  val kind : t -> ([ `Dune | `Opam ], User_message.t list) result
  val kind_exn : t -> [ `Dune | `Opam ] Memo.t
end

val mounted : Context_name.t -> Mounted.t list Memo.t
val find_mounted : Context_name.t -> Package.Name.t -> Mounted.t option Memo.t

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
  val paths : t -> Path.Build.t Opam_build.Paths.t
end

val candidates : Context_name.t -> Candidate.t list Memo.t
val find_candidate : Context_name.t -> Package.Name.t -> Candidate.t option Memo.t
val source : Candidate.t -> Opam_build.Source_input.t Memo.t

val add_artifact_source_rules
  :  dir:Path.Build.t
  -> source_dir:Source_tree.Rules.Dir.t
  -> Build_config.Gen_rules.t
  -> Build_config.Gen_rules.t

open Import

module Identity : sig
  type t

  val workspace : Path.Source.t -> t

  val mounted
    :  lock:Dune_digest.t
    -> package:Package.Name.t
    -> project_root:Path.Local.t
    -> t

  val to_dyn : t -> Dyn.t
end

type t

val create
  :  project:Dune_project.t
  -> identity:Identity.t
  -> source_root:Source_path.t
  -> partition:Build_partition.t
  -> output_root:Path.Build.t
  -> t

val project : t -> Dune_project.t
val identity : t -> Identity.t
val source_root : t -> Source_path.t
val partition : t -> Build_partition.t
val output_root : t -> Path.Build.t
val to_dyn : t -> Dyn.t

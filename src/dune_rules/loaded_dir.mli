open Import

type t

val create : project:Loaded_project.t -> source_dir:Source_path.t -> t
val project : t -> Loaded_project.t
val source_dir : t -> Source_path.t
val relative_dir : t -> Path.Local.t
val output_dir : t -> Path.Build.t
val to_dyn : t -> Dyn.t

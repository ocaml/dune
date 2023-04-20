open Import

val opam_file : Context.t -> Package.t -> Path.Build.t option

val deprecated_meta_file :
  Context.t -> Package.t -> Package.Name.t -> Path.Build.t

val dune_package_file : Context.t -> Package.t -> Path.Build.t

val meta_file : Context.t -> Package.t -> Path.Build.t

val meta_template : Context.t -> Package.t -> Path.Build.t

val deprecated_dune_package_file :
  Context.t -> Package.t -> Package.Name.t -> Path.Build.t

val build_dir : Context.t -> Package.t -> Path.Build.t

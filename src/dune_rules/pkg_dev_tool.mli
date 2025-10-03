open! Import
include module type of Dune_pkg.Dev_tool

val install_path_base_dir_name : string

(** The path to the package universe inside the _build directory
    containing the package dependency closure for the package
    containing the given dev tool *)
val universe_install_path : t -> Path.Build.t

(** The path to the executable for running the given dev tool *)
val exe_path : t -> Path.Build.t

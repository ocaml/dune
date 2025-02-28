open! Import
include module type of Dune_pkg.Dev_tool

val install_path_base_dir_name : string

(** The path to the package universe inside the _build directory
    containing the package dependency closure for the package
    containing the given dev tool *)
val universe_install_path : t -> Path.Build.t

(** The path to the directory inside the _build directory containing
    the installation of the package containing the given dev tool *)
val package_install_path : t -> Path.Build.t

(** The path to the executable for running the given dev tool *)
val exe_path : t -> Path.Build.t

(** The path to the shell executables for running a dev tool with dune *)
val bin_path : unit -> Path.Build.t

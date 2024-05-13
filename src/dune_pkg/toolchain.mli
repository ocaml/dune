open! Import
open! Stdune

module Compiler_package : sig
  (** Names of packages that dune considers to be compiler
      packages. These packages should not be downloaded or built when
      using dune toolchains as the compiler from the toolchain should be
      used instead. *)
  val package_names : Package_name.t list

  (** Constraint to apply to the dependency solver to guarantee a
      solution that's includes a version of a compiler package that's
      supported by dune toolchains. *)
  val constraint_ : Package_dependency.t
end

module Version : sig
  type t

  val latest : t
  val to_string : t -> string
  val all : t list
  val of_package_version : Package_version.t -> t option

  (** The path to the directory containing both the source and binary
      artifacts of this compiler version. *)
  val toolchain_dir : t -> Path.Outside_build_dir.t

  (** The path to the directory containing the binaries contained in
      the compiler toolchain of a given version. Does not guarantee that
      the specified toolchain is installed. *)
  val bin_dir : t -> Path.Outside_build_dir.t

  val is_installed : t -> bool
end

(** Downloads, builds, and installs a compiler toolchain of a given
    version. Performs no actions if the specified toolchain is already
    installed. *)
val get : log:[ `Always | `Never | `Install_only ] -> Version.t -> unit Fiber.t

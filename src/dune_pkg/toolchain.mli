open! Import
open! Stdune

module Version : sig
  type t

  val all_by_string : (string * t) list
  val of_package_version : Package_version.t -> t option

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

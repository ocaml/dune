open! Import
open! Stdune

module Compiler : sig
  type t

  val bin_dir : t -> Path.Outside_build_dir.t
  val get : t -> log_when:[ `Always | `Never | `Install_only ] -> unit Fiber.t
  val constraint_ : Package_dependency.t
end

module Available_compilers : sig
  type t

  val equal : t -> t -> bool
  val load_upstream_opam_repo : unit -> t Fiber.t
  val find_package : t -> Package_name.t -> Package_version.t -> Compiler.t option Fiber.t
end

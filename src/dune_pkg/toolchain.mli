open! Import
open! Stdune

(* [Compiler.t] represents a reference to a compiler installation in a shared
   location.

   It can be retrieved using [get] at which point it will either be returned
   if it is installed or downloaded and built.
*)
module Compiler : sig
  type uninit
  type t

  val bin_dir : t -> Path.Outside_build_dir.t
  val get : uninit -> log_when:[ `Always | `Never | `Install_only ] -> t Fiber.t
end

(* A module that locates a compiler that matches the requested compiler from
   the options available in an Opam repository.
*)
module Available_compilers : sig
  type t

  val equal : t -> t -> bool
  val load_upstream_opam_repo : unit -> t Fiber.t

  val find
    :  t
    -> Package_name.t
    -> Package_version.t
    -> deps:Package_name.t list
    -> Compiler.uninit option Fiber.t
end

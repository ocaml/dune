open! Import
open! Stdune

module Compiler : sig
  (** A particular compiler package that may or may not be
      installed in the user's toolchains directory. *)
  type t

  (** A compiler guaranteed to be installed on the current system in
      the user's toolchains directory. *)
  type installed

  (** The path to the directory containing the compiler's binaries,
      suitable for inclusion in the PATH variable. Guaranteed to
      exist. *)
  val bin_dir : installed -> Path.Outside_build_dir.t

  (** Install the compiler to the user's toolchains directory if it is
      not already installed. *)
  val ensure_installed
    :  t
    -> log_when:[ `Always | `Never | `Install_only ]
    -> installed Fiber.t
end

module Available_compilers : sig
  (** Knows all the compiler packages available in an Opam repository. *)
  type t

  val equal : t -> t -> bool
  val load_upstream_opam_repo : unit -> t Fiber.t

  (** Look up a compiler by Opam package name and version. The name
      must be one of "ocaml-base-compiler" and "ocaml-variants" or [None]
      will be returned. The [deps] argument is used to configure the
      "ocaml-variants" package, as different configuration optians are
      used depending on which of its optional dependencies are
      presenst. *)
  val find
    :  t
    -> Package_name.t
    -> Package_version.t
    -> deps:Package_name.t list
    -> Compiler.t option Fiber.t
end

val find_and_install_toolchain_compiler
  :  Dune_lang.Package_name.t
  -> Package_version.t
  -> deps:Dune_lang.Package_name.t list
  -> Compiler.installed option Fiber.t

val toolchain_base_dir : unit -> Path.Outside_build_dir.t

val bin_dir
  :  Dune_lang.Package_name.t
  -> Package_version.t
  -> deps:Dune_lang.Package_name.t list
  -> Path.Outside_build_dir.t

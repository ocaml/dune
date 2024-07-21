open Import

(** The path to the directory that will contain all toolchain
    versions. Creates the directory if it doesn't already exist. *)
val base_dir : unit -> Path.Outside_build_dir.t

val enabled : bool Config.t

(** When building the compiler with toolchains, build it with [make
      -j] rather than [make -j1], allowing more parallelism. This can
    theoretically lead to build failures, but these are extremely rare in
    practice. *)
val build_compiler_in_parallel : bool Config.t

(** Dune will download and build the ocaml-base-compiler and
    ocaml-variants packages into a user-wide directory (shared among
    projects) rather than using the usual package management mechanism to
    install such packages. Currently compiler packages can't be installed
    by dune's package management features as the compiler is not
    relocatable, and this flag allows dune to workaround this problem
    providing an experience to users that is almost identical to dune
    installing the compiler packgae.

    When this flag is disabled, users of dune package management need to
    manage their compiler installation with opam or a system package
    manager, as compilers packages that would be installed by dune will
    not work correctly. *)
val is_compiler_and_toolchains_enabled : Package.Name.t -> bool

val files : bin_dir:Path.Outside_build_dir.t -> Path.t list Section.Map.t Memo.t

val ocaml
  :  Context_name.t
  -> Env.t
  -> bin_dir:Path.Outside_build_dir.t
  -> Ocaml_toolchain.t Memo.t

val dummy_fetch
  :  target:Path.Build.t
  -> Package.Name.t
  -> Package_version.t
  -> installation_prefix:Path.Outside_build_dir.t
  -> Action.Full.t Action_builder.With_targets.t

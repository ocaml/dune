open Import

(** The path to the directory that will contain all toolchain
    versions. Creates the directory if it doesn't already exist. *)
val base_dir : unit -> Path.Outside_build_dir.t

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

val pkg_dir : Dune_pkg.Lock_dir.Pkg.t -> Path.Outside_build_dir.t

(** Directory that will contain all the installed artifacts of the
    package, suitable for passing as the --prefix argument to a configure
    script. *)
val installation_prefix : pkg_dir:Path.Outside_build_dir.t -> Path.Outside_build_dir.t

val ocaml
  :  Context_name.t
  -> Env.t
  -> bin_dir:Path.Outside_build_dir.t
  -> Ocaml_toolchain.t Memo.t

val modify_install_action
  :  prefix:Path.Outside_build_dir.t
  -> suffix:string
  -> Dune_lang.Action.t
  -> Dune_lang.Action.t Memo.t

val modify_build_action
  :  prefix:Path.Outside_build_dir.t
  -> Dune_lang.Action.t
  -> Dune_lang.Action.t Memo.t

val install_roots
  :  prefix:Path.Outside_build_dir.t
  -> Path.Outside_build_dir.t Install.Roots.t

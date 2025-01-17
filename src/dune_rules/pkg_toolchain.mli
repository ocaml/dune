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

(** Returns the path to the directory containing the given package within the
    toolchain directory. This will be something like
    $XDG_CACHE_DIR/dune/toolchains/ocaml-base-compiler.5.2.1.XXXXXXXX where
    XXXXXXXX is a hash of the package's lockfile. *)
val pkg_dir : Dune_pkg.Lock_dir.Pkg.t -> Path.Outside_build_dir.t

(** Directory that will contain all the installed artifacts of the
    package, suitable for passing as the --prefix argument to a configure
    script. *)
val installation_prefix : pkg_dir:Path.Outside_build_dir.t -> Path.Outside_build_dir.t

(** Rewrite the OCaml compiler install action so that it installs the compiler
    package to the users toolchain directory. If the compiler is found to be
    already installed in the users toolchain directory, the action is instead
    replaced with a no-op. *)
val modify_install_action
  :  prefix:Path.Outside_build_dir.t
  -> suffix:string
  -> Dune_lang.Action.t
  -> Dune_lang.Action.t Memo.t

(** Replace the action with a no-op if the compiler package is already
    installed in the users toolchain directory. *)
val modify_build_action
  :  prefix:Path.Outside_build_dir.t
  -> Dune_lang.Action.t
  -> Dune_lang.Action.t Memo.t

val install_roots
  :  prefix:Path.Outside_build_dir.t
  -> Path.Outside_build_dir.t Install.Roots.t

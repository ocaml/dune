open! Import

(** Ad-hoc feature flags for enabling various features for the dune
    developer preview. These should be false in the upstream version of
    dune and only manually set to true when building dune for the
    developer preview. If you find yourself enabling these when commiting
    to main, instead just remove the flag and permanently enable the
    feature. *)

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
val use_toolchains : bool

(** When building the compiler with toolchains, build it with `make
    -j` rather than `make -j1`, allowing more parallelism. This can
    theoretically lead to build failures, but these are extremely rare in
    practice. *)
val toolchains_build_compiler_in_parallel : bool

open Stdune

(** Setup of dune *)

(** These parameters are set by [ocaml configure.ml] ran by the user or without
    argument as a fallback. *)

(** Where to find installed libraries for the default context. If empty,
    auto-detect it using standard tools such as [ocamlfind]. *)
val library_path : string list

(** Where to install files. All the directories are absolute path *)
val roots : string option Install.Roots.t

val toolchains : Toggle.t
val lock_dev_tool : Toggle.t
val bin_dev_tools : Toggle.t
val portable_lock_dir : Toggle.t
val prefix : string option

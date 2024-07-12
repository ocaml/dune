open! Import
open! Stdune

(** The path to the directory that will contain all toolchain
    versions. Creates the directory if it doesn't already exist. *)
val base_dir : unit -> Path.Outside_build_dir.t

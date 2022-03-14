(** This module contains a small database of flags that is used when compiling C
    and C++ stubs. *)
open! Stdune

open Dune_engine

type phase =
  | Compile
  | Link

(** The name of the file created in the .dune folder after calling the C
    preprocessor *)
val preprocessed_filename : string

type ccomp_type =
  | Gcc
  | Msvc
  | Clang
  | Other of string

(** Get the kind of C processor *)
val ccomp_type : Context.t -> ccomp_type Action_builder.t

(** [get_flags c_compiler] returns the necessary flags to turn this compiler
    into a c++ compiler for some of the most common compilers *)
val get_flags : for_:phase -> Context.t -> string list Action_builder.t

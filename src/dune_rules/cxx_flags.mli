(** This module contains a small database of flags that is used when compiling C
    and C++ stubs. *)

open Import

type phase =
  | Compile
  | Link

(** The detected compiler *)
type ccomp_type =
  | Gcc
  | Msvc
  | Clang
  | Other of string

(** The name of the file created in the .dune folder after calling the C
    preprocessor *)
val preprocessed_filename : string

(** [ccomp_type ctx] returns the C/C++ compiler type. *)
val ccomp_type : Context.t -> ccomp_type Action_builder.t

(** [get_flags for_:phase ctx] returns the necessary flags to turn this compiler
    into a c++ compiler for some of the most common compilers *)
val get_flags : for_:phase -> Context.t -> string list Action_builder.t

(** This module contains a small database of flags that is used when compiling C
    and C++ stubs. *)

open Import

type phase =
  | Compile of Ocaml.Version.t
  | Link

(** The detected compiler *)
type cc_vendor =
  | Gcc
  | Msvc
  | Clang
  | Other of string

(** The name of the file created in the .dune folder after calling the C
    preprocessor *)
val preprocessed_filename : string

(** [cc_vendor ctx] returns the C/C++ compiler vendor. *)
val cc_vendor : Build_context.t -> cc_vendor Action_builder.t

(** [get_flags for_:phase ctx] returns the necessary flags to turn this compiler
    into a c++ compiler for some of the most common compilers *)
val get_flags : for_:phase -> Build_context.t -> string list Action_builder.t

(** [fdiagnostics_color cc] returns the flags activating color diagnostics for
    the C/C++ compiler, if supported. *)
val fdiagnostics_color : cc_vendor -> string list

(** [warnings cc] returns the flags activating the default set of warnings for
    the C/C++ compiler, if supported. *)
val warnings : cc_vendor -> string list

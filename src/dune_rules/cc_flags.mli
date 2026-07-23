(** This module contains a small database of flags that is used when compiling C
    and C++ stubs. *)

open Import

type phase =
  | Compile of Ocaml.Version.t
  | Link

(** The detected compiler. These identifiers are aligned with the OCaml
    compiler's own [OCAML_CC_VENDOR] detection macro. *)
type cc_vendor =
  | Gcc
  | Clang
  | Msvc
  | Mingw
  | Intel
  | Xlc
  | Sun
  | Other of string

(** [cc_vendor ctx] returns the C/C++ compiler vendor. *)
val cc_vendor : Build_context.t -> cc_vendor Action_builder.t

(** [parse_cc_vendor s] maps the vendor word emitted by the detection probe to a
    {!cc_vendor}. Exposed for testing. *)
val parse_cc_vendor : string -> cc_vendor

(** [get_flags for_:phase ctx] returns the necessary flags to turn this compiler
    into a c++ compiler for some of the most common compilers *)
val get_flags : for_:phase -> Build_context.t -> string list Action_builder.t

(** [base_cxx_compile_flags version cc] returns the flags that turn the C
    compiler into a C++ compiler at compile time. Exposed for testing. *)
val base_cxx_compile_flags : Ocaml.Version.t -> cc_vendor -> string list

(** [base_cxx_link_flags cc] returns the flags needed to link C++ stubs. Exposed
    for testing. *)
val base_cxx_link_flags : cc_vendor -> string list

(** [fdiagnostics_color cc] returns the flags activating color diagnostics for
    the C/C++ compiler, if supported. *)
val fdiagnostics_color : cc_vendor -> string list

(** [warnings cc] returns the flags activating the default set of warnings for
    the C/C++ compiler, if supported. *)
val warnings : cc_vendor -> string list

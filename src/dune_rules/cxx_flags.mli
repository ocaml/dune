(** This module contains a small database of flags that is used when compiling C
    and C++ stubs. *)

(** [get_flags c_compiler] returns the necessary flags to turn this compiler
    into a c++ compiler for some of the most common compilers *)
val get_flags : Ocaml_config.Ccomp_type.t -> string list

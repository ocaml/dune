(** Represent the output of [ocamlc -config] *)

open Stdune

type t
val sexp_of_t : t -> Usexp.t

exception E of string

(** Create a [t] value from the output of [ocamlc -config]. Return
    [Error msg] in case of error. *)
val of_lines : string list -> t

(** All the uninterpret variables in the output [ocamlc -config] *)
val bindings : t -> string Map.Make(String).t

(** Lookup a variable in [t] *)
val get : t -> string -> string

(** Same as [get] but return [None] if the variable is not found *)
val get_opt : t -> string -> string option

(** Lookup up a boolean variable in [t]. Returns [false] when the
    variable is not found *)
val get_bool : t -> string -> bool

(** Interpret a varialbe as a list of words separated by spaces *)
val get_strings : t -> string -> string list

val version : t -> int * int * int

(** The full version of the compiler as a string *)
val version_string : t -> string

val natdynlink_supported : t -> bool
val word_size            : t -> string option
val flambda              : t -> bool
val stdlib_dir           : t -> string
val c_compiler           : t -> string
val ocamlc_cflags        : t -> string list
val ocamlopt_cflags      : t -> string list

val ext_obj : t -> string
val ext_asm : t -> string
val ext_lib : t -> string
val ext_dll : t -> string
val ext_exe : t -> string

(** Output of [ocamlc -config] *)

open Import

type t

val sexp_of_t : t -> Sexp.t

val make
  :  ocamlc:Path.t
  -> ocamlc_config_output:string list
  -> t

val read : ocamlc:Path.t -> env:string array -> t Fiber.t

(** Used to pass these settings to jbuild files using the OCaml syntax *)
val ocaml_value : t -> string

val get : ?default:string -> t -> string -> string

val natdynlink_supported : t -> bool
val version : t -> (int * int * int)
val version_string : t -> string
val word_size : t -> string option
val flambda : t -> bool
val stdlib_dir : t -> Path.t
val c_compiler_settings : t -> (string * string * string)

(**/**)
val bindings : t -> string String_map.t

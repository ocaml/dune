(** Version numbers for ocamlc and ocamlopt *)
open! Dune_engine

type t

val make : int * int * int -> t

val of_ocaml_config : Ocaml_config.t -> t

(** Does this support [-no-keep-locs]? *)
val supports_no_keep_locs : t -> bool

(** Does this support [-opaque] for [.mli] files? *)
val supports_opaque_for_mli : t -> bool

(** Does it read the [.cmi] file of module alias even when [-no-alias-deps] is
    passed? *)
val always_reads_alias_cmi : t -> bool

(** Does this support ['color'] in [OCAMLPARAM]? *)
val supports_color_in_ocamlparam : t -> bool

(** Does this support [OCAML_COLOR]? *)
val supports_ocaml_color : t -> bool

(** Does this this support [-args0]? *)
val supports_response_file : t -> bool

(** Does ocamlmklib support [-args0]? *)
val ocamlmklib_supports_response_file : t -> bool

(** Whether [Pervasives] includes the [result] type *)
val pervasives_includes_result : t -> bool

(** Whether the standard library includes the [Uchar] module *)
val stdlib_includes_uchar : t -> bool

(** Whether the standard library includes the [Bigarray] module *)
val stdlib_includes_bigarray : t -> bool

(** Whether the standard library includes the [Seq] module *)
val stdlib_includes_seq : t -> bool

(** Whether ocamlobjinfo supports -no-approx*)
val ooi_supports_no_approx : t -> bool

(** Whether ocamlobjinfo supports -no-code*)
val ooi_supports_no_code : t -> bool

(** Whether the language supports custom let operators *)
val supports_let_syntax : t -> bool

(** Does this support [-output-complete-exe]? *)
val supports_output_complete_exe : t -> bool

(** Whether the compiler supports options for splitting compilation at emit:
    [-stop-after scheduling] [-save-ir-after scheduling] [-start-from emit] *)
val supports_split_at_emit : t -> bool

(** Whether the compiler supports -function-sections *)
val supports_function_sections : t -> bool

(** [-custom] or [-output-complete-exe] depending on the version of OCaml *)
val custom_or_output_complete_exe : t -> string

(** ocamlopt -a always calls the native C linker, even for empty archives *)
val ocamlopt_always_calls_library_linker : t -> bool

(** Whether [Sys.opaque_identity] is in the standard library *)
val has_sys_opaque_identity : t -> bool

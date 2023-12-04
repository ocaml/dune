(** Represent the output of [ocamlc -config] and contents of [Makefile.config].

    This library is internal to dune and guarantees no API stability. *)

(** Represent a parsed and interpreted output of [ocamlc -config] and contents
    of [Makefile.config]. *)
type t

val to_dyn : t Dyn.builder

module Prog_and_args : sig
  type t =
    { prog : string
    ; args : string list
    }
end

(** {1 Raw bindings} *)

(** Represent the parsed but uninterpreted output of [ocamlc -config] or
    contents of [Makefile.config]. *)
module Vars : sig
  type t

  val find : t -> string -> string option
  val of_list_exn : (string * string) list -> t
  val to_list : t -> (string * string) list

  (** Parse the output of [ocamlc -config] given as a list of lines. *)
  val of_lines : string list -> (t, string) result
end

(** {1 Creation} *)

module Origin : sig
  type t =
    | Ocamlc_config
    | Makefile_config of Stdune.Path.t
end

module Os_type : sig
  type t =
    | Win32
    | Unix
    | Other of string

  val to_string : t -> string
end

module Ccomp_type : sig
  type t =
    | Msvc
    | Other of string

  val to_dyn : t -> Dyn.t
  val to_string : t -> string
end

(** Interpret raw bindings (this function also loads the [Makefile.config] file
    in the stdlib directory). *)
val make : Vars.t -> (t, Origin.t * string) result

(** {1 Query} *)

(** The following parameters match the variables in the output of
    [ocamlc -config] but are stable across versions of OCaml. *)

val version : t -> int * int * int
val version_string : t -> string
val standard_library_default : t -> string
val standard_library : t -> string
val standard_runtime : t -> string
val ccomp_type : t -> Ccomp_type.t
val c_compiler : t -> string
val ocamlc_cflags : t -> string list
val ocamlc_cppflags : t -> string list
val ocamlopt_cflags : t -> string list
val ocamlopt_cppflags : t -> string list
val bytecomp_c_compiler : t -> Prog_and_args.t
val bytecomp_c_libraries : t -> string list
val native_c_compiler : t -> Prog_and_args.t
val native_c_libraries : t -> string list
val native_pack_linker : t -> Prog_and_args.t
val cc_profile : t -> string list
val architecture : t -> string
val model : t -> string
val int_size : t -> int
val word_size : t -> int
val system : t -> string
val asm : t -> Prog_and_args.t
val asm_cfi_supported : t -> bool
val with_frame_pointers : t -> bool
val ext_exe : t -> string
val ext_obj : t -> string
val ext_asm : t -> string
val ext_lib : t -> string
val ext_dll : t -> string
val os_type : t -> Os_type.t
val default_executable_name : t -> string
val systhread_supported : t -> bool
val host : t -> string
val target : t -> string
val profiling : t -> bool
val flambda : t -> bool
val spacetime : t -> bool
val safe_string : t -> bool
val exec_magic_number : t -> string
val cmi_magic_number : t -> string
val cmo_magic_number : t -> string
val cma_magic_number : t -> string
val cmx_magic_number : t -> string
val cmxa_magic_number : t -> string
val ast_impl_magic_number : t -> string
val ast_intf_magic_number : t -> string
val cmxs_magic_number : t -> string
val cmt_magic_number : t -> string
val natdynlink_supported : t -> bool
val supports_shared_libraries : t -> bool
val windows_unicode : t -> bool

(** {1 Values} *)

module Value : sig
  type t =
    | Bool of bool
    | Int of int
    | String of string
    | Words of string list
    | Prog_and_args of Prog_and_args.t

  val to_string : t -> string
  val to_dyn : t Dyn.builder
end

val to_list : t -> (string * Value.t) list
val by_name : t -> string -> Value.t option
val is_dev_version : t -> bool

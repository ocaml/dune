open! Dune_engine
open Stdune

type ocaml =
  { ext_lib : string
  ; ext_obj : string
  ; os_type : Ocaml_config.Os_type.t
  ; architecture : string
  ; system : string
  ; model : string  (** Native dynlink *)
  ; ext_dll : string
  ; stdlib_dir : Path.t
  ; ccomp_type : Ocaml_config.Ccomp_type.t
  ; ocaml_version_string : string
  ; ocaml_version : Ocaml_version.t
  }

type t =
  { has_native : bool
  ; natdynlink_supported : Dynlink_supported.By_the_os.t
  ; profile : Profile.t
  ; instrument_with : Lib_name.t list
  ; context_name : Context_name.t
  ; ocaml : ocaml Or_exn.t
  }

val allowed_in_enabled_if : (string * Dune_lang.Syntax.Version.t) list

val get_for_enabled_if : t -> Pform.t -> string Or_exn.t

val linker_can_create_empty_archives : ocaml -> bool

val hash : t -> int

val equal : t -> t -> bool

open Stdune

module Ccomp_type : sig
  type t =
    | Msvc
    | Other of string

  val to_dyn : t -> Dyn.t

  val of_config : Ocaml_config.t -> t
end

type t =
  { has_native : bool
  ; ext_lib : string
  ; ext_obj : string
  ; os_type : string
  ; architecture : string
  ; system : string
  ; model : string  (** Native dynlink *)
  ; natdynlink_supported : Dynlink_supported.By_the_os.t
  ; ext_dll : string
  ; stdlib_dir : Path.t
  ; ccomp_type : Ccomp_type.t
  }

val allowed_in_enabled_if : (string * Dune_lang.Syntax.Version.t) list

val get_for_enabled_if : t -> var:string -> string

val linker_can_create_empty_archives : t -> bool

open! Dune_engine
open! Stdune

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

let allowed_in_enabled_if =
  [ ("architecture", (1, 0))
  ; ("system", (1, 0))
  ; ("model", (1, 0))
  ; ("os_type", (1, 0))
  ; ("ccomp_type", (2, 0))
  ; ("profile", (2, 5))
  ; ("ocaml_version", (2, 5))
  ; ("context_name", (2, 8))
  ]

let get_for_enabled_if t (pform : Pform.t) =
  match pform with
  | Var Architecture -> Result.map t.ocaml ~f:(fun ocaml -> ocaml.architecture)
  | Var System -> Result.map t.ocaml ~f:(fun ocaml -> ocaml.system)
  | Var Model -> Result.map t.ocaml ~f:(fun ocaml -> ocaml.model)
  | Var Os_type ->
    Result.map t.ocaml ~f:(fun ocaml ->
        Ocaml_config.Os_type.to_string ocaml.os_type)
  | Var Ccomp_type ->
    Result.map t.ocaml ~f:(fun ocaml ->
        Ocaml_config.Ccomp_type.to_string ocaml.ccomp_type)
  | Var Profile -> Ok (Profile.to_string t.profile)
  | Var Ocaml_version ->
    Result.map t.ocaml ~f:(fun ocaml -> ocaml.ocaml_version_string)
  | Var Context_name -> Ok (Context_name.to_string t.context_name)
  | _ ->
    Code_error.raise "Lib_config.get_for_enabled_if: var not allowed"
      [ ("var", Pform.to_dyn pform) ]

let linker_can_create_empty_archives ocaml =
  match ocaml.ccomp_type with
  | Msvc -> false
  | Other _ -> true

let hash = Poly.hash

let equal = Poly.equal

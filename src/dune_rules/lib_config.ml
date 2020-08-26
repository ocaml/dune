open! Dune_engine
open! Stdune

type t =
  { has_native : bool
  ; ext_lib : string
  ; ext_obj : string
  ; os_type : Ocaml_config.Os_type.t
  ; architecture : string
  ; system : string
  ; model : string
  ; natdynlink_supported : Dynlink_supported.By_the_os.t
  ; ext_dll : string
  ; stdlib_dir : Path.t
  ; ccomp_type : Ocaml_config.Ccomp_type.t
  ; profile : Profile.t
  ; ocaml_version_string : string
  ; ocaml_version : Ocaml_version.t
  ; instrument_with : Lib_name.t list
  }

let var_map =
  [ ("architecture", fun t -> t.architecture)
  ; ("system", fun t -> t.system)
  ; ("model", fun t -> t.model)
  ; ("os_type", fun t -> Ocaml_config.Os_type.to_string t.os_type)
  ; ("ccomp_type", fun t -> Ocaml_config.Ccomp_type.to_string t.ccomp_type)
  ; ("profile", fun t -> Profile.to_string t.profile)
  ; ("ocaml_version", fun t -> t.ocaml_version_string)
  ]

let allowed_in_enabled_if =
  List.map var_map ~f:(fun (var, _) ->
      let min_version =
        match var with
        | "profile" -> (2, 5)
        | "ccomp_type" -> (2, 0)
        | "ocaml_version" -> (2, 5)
        | _ -> (1, 0)
      in
      (var, min_version))

let get_for_enabled_if t ~var =
  match List.assoc var_map var with
  | Some f -> f t
  | None ->
    Code_error.raise "Lib_config.get_for_enabled_if: var not allowed"
      [ ("var", Dyn.Encoder.string var) ]

let linker_can_create_empty_archives t =
  match t.ccomp_type with
  | Msvc -> false
  | Other _ -> true

open Import

type t =
  { has_native : bool
  ; ext_lib : Filename.Extension.t
  ; ext_obj : Filename.Extension.t
  ; os_type : Ocaml_config.Os_type.t
  ; architecture : string
  ; system : string
  ; model : string
  ; natdynlink_supported : Dynlink_supported.By_the_os.t
  ; ext_dll : string
  ; stdlib_dir : Path.t
  ; ccomp_type : Ocaml_config.Ccomp_type.t
  ; ocaml_version_string : string
  ; ocaml_version : Ocaml.Version.t
  }

let allowed_in_enabled_if =
  [ "architecture", (1, 0)
  ; "system", (1, 0)
  ; "model", (1, 0)
  ; "os_type", (1, 0)
  ; "ccomp_type", (2, 0)
  ; "ocaml_version", (2, 5)
  ]
;;

let get_for_enabled_if t (pform : Pform.t) =
  match pform with
  | Var Architecture -> t.architecture
  | Var System -> t.system
  | Var Model -> t.model
  | Var Os_type -> Ocaml_config.Os_type.to_string t.os_type
  | Var Ccomp_type -> Ocaml_config.Ccomp_type.to_string t.ccomp_type
  | Var Ocaml_version -> t.ocaml_version_string
  | _ ->
    Code_error.raise
      "Lib_config.get_for_enabled_if: var not allowed"
      [ "var", Pform.to_dyn pform ]
;;

let linker_can_create_empty_archives t =
  match t.ccomp_type with
  | Msvc -> false
  | Other _ -> true
;;

let hash = Poly.hash
let equal = Poly.equal
let to_dyn = Dyn.opaque

let cc_g t =
  match t.ccomp_type with
  | Msvc -> []
  | Other _ -> [ "-g" ]
;;

let create ocaml_config ~ocamlopt =
  { has_native = Result.is_ok ocamlopt
  ; ext_obj = Ocaml_config.ext_obj ocaml_config
  ; ext_lib = Ocaml_config.ext_lib ocaml_config
  ; os_type = Ocaml_config.os_type ocaml_config
  ; architecture = Ocaml_config.architecture ocaml_config
  ; system = Ocaml_config.system ocaml_config
  ; model = Ocaml_config.model ocaml_config
  ; ext_dll = Ocaml_config.ext_dll ocaml_config
  ; natdynlink_supported =
      (let natdynlink_supported = Ocaml_config.natdynlink_supported ocaml_config in
       Dynlink_supported.By_the_os.of_bool natdynlink_supported)
  ; stdlib_dir = Path.of_string (Ocaml_config.standard_library ocaml_config)
  ; ccomp_type = Ocaml_config.ccomp_type ocaml_config
  ; ocaml_version_string = Ocaml_config.version_string ocaml_config
  ; ocaml_version = Ocaml.Version.of_ocaml_config ocaml_config
  }
;;

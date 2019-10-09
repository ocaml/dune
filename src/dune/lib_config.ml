open! Stdune

type t =
  { has_native : bool
  ; ext_lib : string
  ; ext_obj : string
  ; os_type : string
  ; architecture : string
  ; system : string
  ; model : string
  ; natdynlink_supported : Dynlink_supported.By_the_os.t
  ; ext_dll : string
  ; stdlib_dir : Path.t
  ; ccomp_type : string
  }

let var_map =
  [ ("architecture", fun t -> t.architecture)
  ; ("system", fun t -> t.system)
  ; ("model", fun t -> t.model)
  ; ("os_type", fun t -> t.os_type)
  ]

let allowed_in_enabled_if = List.map ~f:fst var_map

let get_for_enabled_if t ~var =
  match List.assoc var_map var with
  | Some f -> f t
  | None ->
    Code_error.raise "Lib_config.get_for_enabled_if: var not allowed"
      [ ("var", Dyn.Encoder.string var) ]

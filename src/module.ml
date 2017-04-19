open Import

type t =
  { name       : string
  ; impl_fname : string
  ; intf_fname : string option
  ; obj_name   : string
  ; reason     : bool
  }

let real_unit_name t = String.capitalize_ascii (Filename.basename t.obj_name)

let file t ~dir (kind : Ml_kind.t) =
  match kind with
  | Impl -> Some (Path.relative dir t.impl_fname)
  | Intf -> Option.map t.intf_fname ~f:(Path.relative dir)

let cm_source t ~dir kind = file t ~dir (Cm_kind.source kind)

let cm_file t ~dir kind = Path.relative dir (t.obj_name ^ Cm_kind.ext kind)

let cmt_file t ~dir (kind : Ml_kind.t) =
  match kind with
  | Impl -> Some (Path.relative dir (t.obj_name ^ ".cmt"))
  | Intf -> Option.map t.intf_fname ~f:(fun _ -> Path.relative dir (t.obj_name ^ ".cmti"))

let ocaml_of_reason t =
  if t.reason then
    { t with
      reason = false
    ; impl_fname = Filename.chop_extension t.impl_fname ^ ".ml"
    ; intf_fname = Option.map t.intf_fname
                    ~f:(fun mli -> Filename.chop_extension mli ^ ".mli")
    }
  else
    invalid_arg "ocaml_of_reason only expect reason sources"

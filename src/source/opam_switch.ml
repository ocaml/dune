open Import

type t =
  { root : string option
  ; switch : string
  }

let repr =
  Repr.record
    "opam-switch"
    [ Repr.field "root" (Repr.option Repr.string) ~get:(fun t -> t.root)
    ; Repr.field "switch" Repr.string ~get:(fun t -> t.switch)
    ]
;;

let to_dyn = Repr.to_dyn repr

include Repr.Poly (struct
    type nonrec t = t

    let repr = repr
  end)

let opam_switch_prefix_var_name = "OPAM_SWITCH_PREFIX"

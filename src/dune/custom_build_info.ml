type t =
  { max_size : int
  ; action : Action_dune_lang.t
  }

let output_file = "custom_build_info.txt-gen"

let action =
  let open Dune_lang.Decoder in
  field "action" Action_dune_lang.decode

let decode () =
  let open Dune_lang.Decoder in
  field_o "custom_build_info"
    (fields
       (let+ max_size = field "max_size" int
        and+ action = action in
        { max_size; action }))

open! Stdune

type t =
  { max_size : int
  ; action : Loc.t * Action_dune_lang.t
  }

let output_file = "custom_build_info.txt-gen"

let decode () =
  let open Dune_lang.Decoder in
  field_o "custom_build_info"
    (fields
       (let+ max_size = field "max_size" int
        and+ action = field "action" (located Action_dune_lang.decode) in
        { max_size; action }))

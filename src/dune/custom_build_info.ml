open Import

type t =
  { max_size : int
  ; action : Path.Build.t -> Action.t
  }

let output_file = "custom_build_info.txt-gen"

let action =
  let open Dune_lang.Decoder in
  field "action"
    (let+ action = Action.decode in
     fun dir ->
       let target = Path.Build.relative dir output_file in
       Action.with_stdout_to target action)

let decode () =
  let open Dune_lang.Decoder in
  field_o "custom_build_info"
    (fields
       (let+ max_size = field "max_size" int
        and+ action = action in
        { max_size; action }))

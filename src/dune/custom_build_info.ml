open Import

type t = (Path.Build.t -> Action.t) option

let output_file = "custom_build_info.txt-gen"

let decode () =
  let open Dune_lang.Decoder in
  field_o "custom_build_info"
    (fields
       (field "action"
          (let+ action = Action.decode in
           fun dir ->
             let target = Path.Build.relative dir output_file in
             Action.with_stdout_to target action)))

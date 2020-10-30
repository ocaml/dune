open! Dune_engine

type t =
  { name : string
  ; pkg_config_name : string option
  ; c_headers : string option
  ; generated_modules : string list
  }

let name = "ctypes"

type Stanza.t += T of t

let decode =
  let open Dune_lang.Decoder in
  fields
    (let+ name = field "name" string
     and+ pkg_config_name = field_o "pkg_config_name" string
     and+ c_headers = field_o "c_headers" string
     and+ generated_modules = field "generated_modules" (repeat string)
     in
     { name; pkg_config_name; c_headers; generated_modules })

let syntax =
  Dune_lang.Syntax.create ~name ~desc:"the ctypes extension"
    (* XXX: insert the latest version of dune language *)
    [ ((0, 1), `Since (2, 8))
    ]

let () =
  let open Dune_lang.Decoder in
  Dune_project.Extension.register_simple syntax
    (return [ (name, decode >>| fun x -> [ T x ]) ])

module Configurator = Configurator.V1

let () =
  let module C_define = Configurator.C_define in
  Configurator.main ~name:"c_test" (fun t ->
    C_define.import t
      ~includes:["caml/config.h"]
      [ "CAML_CONFIG_H", C_define.Type.Switch
      ; "Page_log", C_define.Type.Int
      ; "__func__", C_define.Type.String
      ]
    |> List.iter (fun (n, v) ->
      Printf.printf "%s=%s\n"
        n (match v with
          | C_define.Value.String s -> s
          | Int i -> string_of_int i
          | Switch b -> string_of_bool b
        )
    )
  )

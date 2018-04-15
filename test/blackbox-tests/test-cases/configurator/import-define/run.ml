module Configurator = Configurator.V1

let () =
  let module C_define = Configurator.C_define in
  Configurator.main ~name:"c_test" (fun t ->
    assert (
      C_define.import t
        ~includes:["caml/config.h"]
        [ "CAML_CONFIG_H", C_define.Type.Switch
        ; "Page_log", C_define.Type.Int
        ] =
      [ "CAML_CONFIG_H", C_define.Value.Switch true
      ; "Page_log", Int 12
      ]
    );
    print_endline "Successfully import #define's"
  )

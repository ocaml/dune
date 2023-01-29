module C = Configurator.V1

let () =
  C.main ~name:"taglib-pkg-config" (fun c ->
    C.C_define.gen_header_file c ~fname:"config.h" [];
    C.Flags.write_sexp "c_flags.sexp" []
  )

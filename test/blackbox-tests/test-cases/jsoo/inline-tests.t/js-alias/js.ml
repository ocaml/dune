let _ =
  Printf.eprintf
    "inline tests (%s - alias)\n"
    (match Sys.backend_type with
     | Native -> "Native"
     | Bytecode -> "Byte"
     | Other _ -> "JS")
;;

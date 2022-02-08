let () =
  if Sys.argv.(1) = "macosx" then
    Printf.printf
      {|(-cclib "-framework Foundation" -cclib "-framework CoreServices")|}
  else print_string "()"

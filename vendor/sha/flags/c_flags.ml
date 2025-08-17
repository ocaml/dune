let flags =
  match Sys.win32 with
  | true  -> "()"
  | false -> "(-Wall -O3 -funroll-loops)"

let () =
  print_endline flags

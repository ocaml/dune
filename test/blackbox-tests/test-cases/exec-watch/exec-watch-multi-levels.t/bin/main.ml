let () =
  let touch path =
    let fd = Unix.openfile path [ Unix.O_CREAT; O_CLOEXEC ] 0o777 in
    Unix.close fd
  in
  print_endline "foo";
  match Sys.argv.(1)  with
  | exception _ -> ()
  | path -> touch path

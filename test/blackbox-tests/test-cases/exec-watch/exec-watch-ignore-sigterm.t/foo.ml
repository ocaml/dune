let touch path =
  let fd = Unix.openfile path [ Unix.O_CREAT ] 777 in
  Unix.close fd

let () =
  let _ = Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigterm ] in
  print_endline "1: before";
  touch "_build/done_flag";
  Unix.sleep 1000

external getpgid : int -> int = "test_getpgid"

let () =
  if not Sys.win32
  then (
    let pid = Unix.getpid () in
    let pgid = getpgid pid in
    if pid <> pgid then failwith "pgid and pid not equal")
;;

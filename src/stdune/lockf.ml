type t = Unix.file_descr

let _lock op path =
  Path.mkdir_p (Path.parent_exn (Path.of_string path)) ;
  let fd = Unix.openfile path [Unix.O_CREAT; Unix.O_RDWR] 0o600
  and pid = string_of_int (Unix.getpid ()) in
  let pid_len = String.length pid in
  (* fcntl would enable to downgrade the lock to a read lock so
    other process can read the PID, but it is not bound in Unix *)
  try
    Unix.lockf fd op 0 ;
    if Unix.single_write_substring fd pid 0 pid_len <> pid_len then (
      Unix.close fd ;
      failwith "Unable to write PID to lock file" )
    else Some fd
  with
  | Unix.Unix_error (Unix.EAGAIN, _, _) ->
      Unix.close fd ; None
  | e ->
      Unix.close fd ; raise e

let lock path =
  match _lock Unix.F_LOCK path with
  | Some fd ->
      fd
  | None ->
      failwith "lockf F_LOCK didn't lock"

let lock_try = _lock Unix.F_TLOCK

let unlock fd = Unix.close fd

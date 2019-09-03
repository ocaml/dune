type t = Unix.file_descr

let create_internal op path =
  Path.mkdir_p (Path.parent_exn path);
  let fd =
    Unix.openfile (Path.to_string path) [ Unix.O_CREAT; Unix.O_RDWR ] 0o600
  and pid = string_of_int (Unix.getpid ()) in
  let pid_len = String.length pid in
  (* fcntl would enable to downgrade the lock to a read lock so other process
     can read the PID, but it is not bound in Unix *)
  match
    Unix.lockf fd op 0;
    assert (Unix.single_write_substring fd pid 0 pid_len = pid_len)
  with
  | () -> Some fd
  | exception Unix.Unix_error (EAGAIN, _, _) ->
    Unix.close fd;
    None
  | exception e ->
    Unix.close fd;
    Exn.reraise e

let create path = Option.value_exn (create_internal F_LOCK path)

let try_create = create_internal F_TLOCK

let unlock fd = Unix.close fd

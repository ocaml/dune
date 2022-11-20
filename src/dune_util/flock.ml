type t = Unix.file_descr

let create x = x

type lock =
  | Shared
  | Exclusive

let is_exclusive = function
  | Exclusive -> true
  | Shared -> false

let lock_block t lock =
  match Unix.lockf t (if is_exclusive lock then F_LOCK else F_RLOCK) 0 with
  | () -> Ok ()
  | exception Unix.Unix_error (err, _, _) -> Error err

let lock_non_block t lock =
  match Unix.lockf t (if is_exclusive lock then F_TLOCK else F_TRLOCK) 0 with
  | () -> Ok `Success
  | exception Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EACCES), _, _) ->
    Ok `Failure
  | exception Unix.Unix_error (err, _, _) -> Error err

let unlock t =
  match Unix.lockf t F_ULOCK 0 with
  | () -> Ok ()
  | exception Unix.Unix_error (err, _, _) -> Error err

let fd x = x

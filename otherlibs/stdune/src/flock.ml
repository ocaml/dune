type t = Fd.t

let create x = x

external gen_lock
  :  Unix.file_descr
  -> block:bool
  -> exclusive:bool
  -> unit
  = "dune_flock_lock"

type lock =
  | Shared
  | Exclusive

let is_exclusive = function
  | Exclusive -> true
  | Shared -> false
;;

let lock_block t lock =
  match
    gen_lock (Fd.unsafe_to_unix_file_descr t) ~block:true ~exclusive:(is_exclusive lock)
  with
  | () -> Ok ()
  | exception Unix.Unix_error (err, _, _) -> Error err
;;

let lock_non_block t lock =
  match
    gen_lock (Fd.unsafe_to_unix_file_descr t) ~block:false ~exclusive:(is_exclusive lock)
  with
  | () -> Ok `Success
  | exception Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EACCES), _, _) -> Ok `Failure
  | exception Unix.Unix_error (err, _, _) -> Error err
;;

external unlock_raw : Unix.file_descr -> unit = "dune_flock_unlock"

let unlock t =
  match unlock_raw (Fd.unsafe_to_unix_file_descr t) with
  | () -> Ok ()
  | exception Unix.Unix_error (err, _, _) -> Error err
;;

let fd x = x

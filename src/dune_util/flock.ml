type t = Unix.file_descr

let create x = x

external gen_lock : t -> block:bool -> exclusive:bool -> unit
  = "dune_flock_lock"

type lock =
  | Shared
  | Exclusive

let is_exculsive = function
  | Exclusive -> true
  | Shared -> false

let lock_block t lock =
  match gen_lock t ~block:true ~exclusive:(is_exculsive lock) with
  | () -> Ok ()
  | exception Unix.Unix_error (err, _, _) -> Error err

let lock_non_block t lock =
  match gen_lock t ~block:false ~exclusive:(is_exculsive lock) with
  | () -> Ok `Success
  | exception Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) -> Ok `Failure
  | exception Unix.Unix_error (err, _, _) -> Error err

external unlock : t -> unit = "dune_flock_unlock"

let unlock t =
  match unlock t with
  | () -> Ok ()
  | exception Unix.Unix_error (err, _, _) -> Error err

let fd x = x

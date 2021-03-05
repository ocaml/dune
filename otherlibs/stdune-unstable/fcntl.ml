type lock_type =
  | Read
  | Write

type lock_operation =
  | F_SETLK
  | F_SETLKW
  | F_GETLK

type l_type =
  | F_RDLCK
  | F_WRLCK
  | F_UNLCK

type whence =
  | Set
  | Cur
  | End

external fcntl_lk :
     Unix.file_descr
  -> lock_operation
  -> l_type
  -> whence
  -> int
  -> int
  -> int * int = "fcntl_lk_bytecode" "fcntl_lk_native"

let of_type = function
  | Read -> F_RDLCK
  | Write -> F_WRLCK

let lock ?(whence = Set) ?(start = 0) ?(len = 0) fd t =
  match fcntl_lk fd F_SETLKW (of_type t) whence start len with
  | -1, _ -> raise (Unix.Unix_error (Unix.EINTR, "fcntl", "F_SETLKW"))
  | 0, _ -> ()
  | r, _ ->
    failwith (Printf.sprintf "unexpected fcntl(F_SETLKW) return code: %i" r)

let lock_try ?(whence = Set) ?(start = 0) ?(len = 0) fd t =
  match fcntl_lk fd F_SETLK (of_type t) whence start len with
  | -1, _ -> false
  | 0, _ -> true
  | r, _ ->
    failwith (Printf.sprintf "unexpected fcntl(F_SETLK) return code: %i" r)

let lock_get ?(whence = Set) ?(start = 0) ?(len = 0) fd t =
  match fcntl_lk fd F_GETLK (of_type t) whence start len with
  | 0, _ -> None
  | 1, pid -> Some (Read, pid)
  | 2, pid -> Some (Write, pid)
  | r, _ ->
    failwith (Printf.sprintf "unexpected fcntl(F_GETLK) return code: %i" r)

let unlock ?(whence = Set) ?(start = 0) ?(len = 0) fd =
  match fcntl_lk fd F_SETLKW F_UNLCK whence start len with
  | 0, _ -> ()
  | r, _ ->
    failwith
      (Printf.sprintf "unexpected fcntl(F_SETLK, F_UNLCK) return code: %i" r)

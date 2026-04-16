type t =
  { fd : Unix.file_descr
  ; mutable closed : bool
  }

let unsafe_to_int (fd : Unix.file_descr) = (Obj.magic fd : int)
let equal_raw_fd = Poly.equal
let hash_raw_fd fd = Int.hash (unsafe_to_int fd)
let raw_fd_repr = Repr.view Repr.int ~to_:unsafe_to_int
let unsafe_to_unix_file_descr t = t.fd
let unsafe_of_unix_file_descr fd = { fd; closed = false }

let close t =
  if not t.closed
  then (
    Unix.close t.fd;
    t.closed <- true)
;;

let repr =
  Repr.record
    "Fd.t"
    [ Repr.field "fd" raw_fd_repr ~get:(fun t -> t.fd)
    ; Repr.field "closed" Repr.bool ~get:(fun t -> t.closed)
    ]
;;

let equal t1 t2 = equal_raw_fd t1.fd t2.fd
let hash t = hash_raw_fd t.fd
let to_dyn = Repr.to_dyn repr

type lock_type =
  | Read
  | Write

type whence =
  | Set
  | Cur
  | End

val lock :
     ?whence:whence
  -> ?start:int
  -> ?len:int
  -> Unix.file_descr
  -> lock_type
  -> unit

val lock_try :
     ?whence:whence
  -> ?start:int
  -> ?len:int
  -> Unix.file_descr
  -> lock_type
  -> bool

val lock_get :
     ?whence:whence
  -> ?start:int
  -> ?len:int
  -> Unix.file_descr
  -> lock_type
  -> (lock_type * int) option

val unlock : ?whence:whence -> ?start:int -> ?len:int -> Unix.file_descr -> unit

(** Wrapper around [flock]. Implements dune's global locking. Mostly exposed for
    testing *)

type t

val fd : t -> Unix.file_descr

val create : Unix.file_descr -> t

type lock =
  | Shared
  | Exclusive

val lock_block : t -> lock -> (unit, Unix.error) result

val lock_non_block : t -> lock -> ([ `Success | `Failure ], Unix.error) result

val unlock : t -> (unit, Unix.error) result

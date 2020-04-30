(** Monad signatures *)

module Make (M : Monad_intf.S1_base) : Monad_intf.S1 with type 'a t := 'a M.t

module Id : Monad_intf.S1 with type 'a t = 'a

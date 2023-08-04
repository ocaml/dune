(** Unix Signal helpers *)

type t =
  | Int
  | Term
  | Abrt
  | Alrm
  | Fpe
  | Hup
  | Ill
  | Kill
  | Pipe
  | Quit
  | Segv
  | Usr1
  | Usr2
  | Chld
  | Cont
  | Stop
  | Tstp
  | Ttin
  | Ttou
  | Vtalrm
  | Prof
  | Bus
  | Poll
  | Sys
  | Trap
  | Urg
  | Xcpu
  | Xfsz
  | Winch
  | Unknown of int

(** Get the signal's name, e.g. [Int] maps to "INT", [Term] to "TERM", etc. *)
val name : t -> string

val to_int : t -> int
val to_dyn : t -> Dyn.t
val compare : t -> t -> Ordering.t
val of_int : int -> t

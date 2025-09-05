type t = Unix.error

val equal : t -> t -> bool

(** A Unix error along with the corresponding system call and argument, as
      thrown in Unix exceptions. *)
module Detailed : sig
  type nonrec t = t * string * string

  val to_dyn : t -> Dyn.t
  val raise : t -> 'a
  val create : Unix.error -> syscall:string -> arg:string -> t

  (** Apply a function to an argument, catching a detailed Unix error. *)
  val catch : ('a -> 'b) -> 'a -> ('b, t) result

  val equal : t -> t -> bool
  val to_string_hum : t -> string
  val pp : ?prefix:string -> t -> 'a Pp.t
end

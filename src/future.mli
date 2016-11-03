(** Simplified Async/Lwt like monad *)

type 'a t

val return : 'a -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val all : 'a t list -> 'a list t
val all_unit : unit t list -> unit t

(** [run ?stdout_to prog args] spawns a sub-process and wait for its termination *)
val run : ?stdout_to:string -> string -> string list -> unit t

module Scheduler : sig
  val go : 'a t -> 'a
end

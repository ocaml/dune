(** Monad signatures *)

module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Id : S with type 'a t = 'a

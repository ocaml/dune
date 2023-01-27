module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

  val parallel_iter : (unit -> 'a option t) -> f:('a -> unit t) -> unit t

  val finalize : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

  module O : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  end

  module Ivar : sig
    type 'a fiber := 'a t

    type 'a t

    val create : unit -> 'a t

    val read : 'a t -> 'a fiber

    val fill : 'a t -> 'a -> unit fiber
  end
end

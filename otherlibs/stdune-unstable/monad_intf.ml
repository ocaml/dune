(** This module type is accessible as [Stdune.Monad.Basic] outside of [Stdune]. *)
module type Basic = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> f:('a -> 'b t) -> 'b t
end

(** This module type is accessible as just [Stdune.Monad] outside of [Stdune]. *)
module type S = sig
  include Basic

  val map : 'a t -> f:('a -> 'b) -> 'b t

  module O : sig
    val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

    val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( >>> ) : unit t -> 'a t -> 'a t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
  end
end

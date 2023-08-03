(** This module type is accessible as just [Stdune.Applicative.Basic] outside of
    [Stdune]. *)
module type Basic = sig
  type 'a t

  val return : 'a -> 'a t
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
end

(** This module type is accessible as just [Stdune.Applicative] outside of
    [Stdune]. *)
module type S = sig
  include Basic

  val all : 'a t list -> 'a list t

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val ( >>> ) : unit t -> 'a t -> 'a t
  end
end

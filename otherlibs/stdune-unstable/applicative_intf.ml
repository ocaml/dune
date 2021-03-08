module type S1_base = sig
  type 'a t

  val return : 'a -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val both : 'a t -> 'b t -> ('a * 'b) t
end

module type S1 = sig
  include S1_base

  val all : 'a t list -> 'a list t

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

    val ( >>> ) : unit t -> 'a t -> 'a t
  end
end

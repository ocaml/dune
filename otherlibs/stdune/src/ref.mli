type 'a t = 'a ref

module With_freeze : sig
  (* A reference that can be frozen. After freezing, it becomes read only *)

  type 'a t

  val create : 'a -> 'a t
  val set : 'a t -> 'a -> unit
  val get : 'a t -> 'a
  val freeze : _ t -> unit
end

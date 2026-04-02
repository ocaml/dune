type _ t
type 'a repr = 'a t
type 'a field
type 'a case

module type S = sig
  type t

  val repr : t repr
end

module type S1 = sig
  type 'a t

  val repr : 'a repr -> 'a t repr
end

module type S2 = sig
  type ('a, 'b) t

  val repr : 'a repr -> 'b repr -> ('a, 'b) t repr
end

module T3 : sig
  type ('a, 'b, 'c) t = 'a * 'b * 'c

  val repr : 'a repr -> 'b repr -> 'c repr -> ('a, 'b, 'c) t repr
end

module T4 : sig
  type ('a, 'b, 'c, 'd) t = 'a * 'b * 'c * 'd

  val repr : 'a repr -> 'b repr -> 'c repr -> 'd repr -> ('a, 'b, 'c, 'd) t repr
end

val to_dyn : 'a repr -> 'a -> Dyn.t
val unit : unit t
val bool : bool t
val int : int t
val string : string t
val option : 'a t -> 'a option t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t
val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val fix : ('a t -> 'a t) -> 'a t
val view : 'b t -> to_:('a -> 'b) -> 'a t
val field : string -> 'b t -> get:('a -> 'b) -> 'a field
val record : string -> 'a field list -> 'a t
val case : string -> 'b t -> proj:('a -> 'b option) -> 'a case
val case0 : string -> test:('a -> bool) -> 'a case
val variant : string -> 'a case list -> 'a t
val abstract : ('a -> Dyn.t) -> 'a t

module Make (T : S) : sig
  val to_dyn : T.t -> Dyn.t
end

module Make1 (T : S1) : sig
  val to_dyn : ('a -> Dyn.t) -> 'a T.t -> Dyn.t
end

module Make2 (T : S2) : sig
  val to_dyn : ('a -> Dyn.t) -> ('b -> Dyn.t) -> ('a, 'b) T.t -> Dyn.t
end

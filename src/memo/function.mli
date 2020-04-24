open Stdune

module Type : sig
  type ('a, 'b, 'f) t =
    | Sync : ('a, 'b, 'a -> 'b) t
    | Async : ('a, 'b, 'a -> 'b Fiber.t) t
end

module Info : sig
  type t

  val named : ?doc:string -> name:string -> unit -> t

  val doc : t -> string option

  val name : t -> string option

  val of_loc : Loc.t -> t

  val to_dyn : t -> Dyn.t
end

type ('a, 'b, 'f) t =
  | Sync : ('a -> 'b) -> ('a, 'b, 'a -> 'b) t
  | Async : ('a -> 'b Fiber.t) -> ('a, 'b, 'a -> 'b Fiber.t) t

val of_type : ('a, 'b, 'c) Type.t -> 'c -> ('a, 'b, 'c) t

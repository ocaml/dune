open Stdune

module Lock : sig
  type t
  val create : Path.Build.t -> t
  val with_lock : t -> f:(unit -> 'a Fiber.t) -> 'a Fiber.t
end

val t : Lock.t
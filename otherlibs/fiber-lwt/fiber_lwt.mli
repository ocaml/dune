(** Lwt integration for fibers *)

module Fiber_inside_lwt : sig
  val run : 'a Fiber.t -> 'a Lwt.t

  (** Should only be called inside a [run] *)
  val callback_to_lwt : (unit -> 'a Lwt.t) -> ('a, exn) result Fiber.t
end

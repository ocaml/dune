open Stdune

module type Session = sig
  type t

  (** [close t] closes the session *)
  val close : t -> unit Fiber.t

  (** [write t x] writes the s-expression *)
  val write : t -> Sexp.t list -> (unit, [ `Closed ]) result Fiber.t

  (** [read t] attempts to read from [t]. If an s-expression is read, it is
        returned as [Some sexp], otherwise [None] is returned and the session is
        closed. *)
  val read : t -> Sexp.t option Fiber.t

  (* [name t] returns the name of the endpoint the session is connected to. *)
  val name : t -> string
end

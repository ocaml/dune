open Stdune

val fetch : OpamUrl.t -> target:Path.t -> unit Fiber.t

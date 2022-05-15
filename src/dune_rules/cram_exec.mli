open Import

val run : env:Env.t -> script:Path.t -> unit Fiber.t

val linkme : unit

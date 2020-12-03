open! Build_api.Api
open Stdune

val run : env:Env.t -> script:Path.t -> unit Fiber.t

val linkme : unit

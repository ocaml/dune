open! Dune_engine
open Stdune

val run : env:Env.t -> script:Path.t -> unit Fiber.t

val linkme : unit

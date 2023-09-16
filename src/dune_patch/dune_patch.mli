open Stdune
open Dune_engine

val action : patch:Path.t -> Action.t

module For_tests : sig
  val exec
    :  Dune_engine.Display.t
    -> patch:Path.t
    -> dir:Path.t
    -> stderr:Dune_engine.Process.Io.output Dune_engine.Process.Io.t
    -> unit Fiber.t
end

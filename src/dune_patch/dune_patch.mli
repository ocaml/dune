open Stdune
open Dune_engine

val action : patch:Path.t -> Action.t

module For_tests : sig
  module Patch : sig
    type t

    val repr : t Repr.t
  end

  val patches_of_string : string -> Patch.t list

  val exec
    :  Dune_engine.Display.t
    -> patch:Path.t
    -> dir:Path.t
    -> stderr:Dune_engine.Process.Io.output Dune_engine.Process.Io.t
    -> unit Fiber.t
end

open Stdune
open Dune_engine

val action : display:Display.t -> patch_prog:Path.t -> input:Path.t -> Action.t

module Spec : sig
  type ('path, 'target) t =
    { patch_file : 'path
    ; display : Dune_engine.Display.t
    ; patch_prog : Path.t
    }

  val action
    :  (Path.t, 'a) t
    -> ectx:Action.Ext.context
    -> eenv:Action.Ext.env
    -> unit Fiber.t
end

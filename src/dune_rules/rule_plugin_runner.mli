open Import

module Registry : sig
  type entry =
    { dir : Path.Source.t
    ; project : Dune_project.t
    ; definition : Rule_plugin.Definition.t
    }

  type t

  val create : entry list -> t

  val expand
    :  t
    -> context:Context_name.t
    -> project:Dune_project.t
    -> dir:Path.Source.t
    -> Rule_plugin.Invocation.t
    -> Dune_lang.Ast.t list Memo.t
end

val run : unit -> unit Fiber.t
val stop : unit -> unit Fiber.t

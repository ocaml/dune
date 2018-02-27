(** ocamldep managenemt *)

open Import

module Dep_graph : sig
  type t

  val deps_of
    :  t
    -> Module.t
    -> (unit, Module.t list) Build.t

  val top_closed_implementations
    :  t
    -> Module.t list
    -> (unit, Module.t list) Build.t
end

module Dep_graphs : sig
  type t = Dep_graph.t Ml_kind.Dict.t

  val dummy : Module.t -> t
end

(** Generate ocamldep rules for the given modules. [item] is either
    the internal name of a library of the first name of a list of
    executables.

    For wrapped libraries, [lib_interface_module] is the main module
    of the library.

    [already_used] represents the modules that are used by another
    stanzas in the same directory. No [.d] rule will be generated for
    such modules.

    Return arrows that evaluate to the dependency graphs.  *)
val rules
  :  dir:Path.t
  -> modules:Module.t String_map.t
  -> ?already_used:String_set.t
  -> alias_module:Module.t option
  -> lib_interface_module:Module.t option
  -> Super_context.t
  -> Dep_graphs.t

(** OCaml module compilation *)

open Import

module Includes : sig
  type t

  val make : Super_context.t -> requires:Lib.t list Or_exn.t -> t

  (** Empty set of include directories *)
  val empty : t
end

(** Setup rules to build a single module. *)
val build_module
  :  Super_context.t
  -> ?sandbox:bool
  -> dynlink:bool
  -> ?js_of_ocaml:Jbuild.Js_of_ocaml.t
  -> flags:Ocaml_flags.t
  -> Module.t
  -> scope:Scope.t
  -> dir:Path.t
  -> obj_dir:Path.t
  -> dep_graphs:Ocamldep.Dep_graphs.t
  -> includes:Includes.t
  -> alias_module:Module.t option
  -> unit

(** Setup rules to build all of [modules] *)
val build_modules
  :  Super_context.t
  -> dynlink:bool
  -> js_of_ocaml:Jbuild.Js_of_ocaml.t
  -> flags:Ocaml_flags.t
  -> scope:Scope.t
  -> dir:Path.t
  -> obj_dir:Path.t
  -> dep_graphs:Ocamldep.Dep_graphs.t
  -> modules:Module.t Module.Name.Map.t
  -> includes:Includes.t
  -> alias_module:Module.t option
  -> unit

(** Odoc rules *)

open Jbuild

val setup_library_rules
  :  Super_context.t
  -> Library.t
  -> dir:Path.t
  -> scope:Scope.t
  -> modules:Module.t Module.Name.Map.t
  -> mld_files:string list
  -> requires:(unit, Lib.t list) Build.t
  -> dep_graphs:Ocamldep.Dep_graphs.t
  -> unit

val gen_rules : Super_context.t -> dir:Path.t -> string list -> unit

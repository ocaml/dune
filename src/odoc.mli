(** Odoc rules *)

open Jbuild

module Gen (S : sig val sctx : Super_context.t end) : sig

  val setup_library_odoc_rules
    :  Library.t
    -> scope:Scope.t
    -> modules:Module.t Module.Name.Map.t
    -> requires:(unit, Lib.t list) Build.t
    -> dep_graphs:Ocamldep.Dep_graphs.t
    -> unit

  val init
    :  modules_by_lib:(dir:Path.t -> Library.t -> Module.t list)
    -> mlds_of_dir:(Documentation.t -> dir:Path.t -> Path.t list)
    -> unit

  val gen_rules : dir:Path.t -> string list -> unit
end

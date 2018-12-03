(** Odoc rules *)

open! Stdune
open Import
open Dune_file

module Gen (S : sig val sctx : Super_context.t end) : sig

  val setup_library_odoc_rules
    :  Library.t
    -> scope:Scope.t
    -> modules:Module.t Module.Name.Map.t
    -> requires:Lib.t list Or_exn.t
    -> dep_graphs:Dep_graph.Ml_kind.t
    -> unit

  val init : unit -> unit

  val gen_rules : dir:Path.t -> string list -> unit
end

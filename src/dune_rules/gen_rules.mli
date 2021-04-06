open! Dune_engine
open! Stdune
open! Import

(* Set the rule generator callback. Returns evaluated Dune files per context
   names. *)
val init :
     contexts:Context.t list
  -> only_packages:Package.t Package.Name.Map.t option
  -> Dune_load.conf
  -> Super_context.t Context_name.Map.t Fiber.t

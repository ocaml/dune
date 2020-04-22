open! Stdune
open! Import

(* Generate rules. Returns evaluated Dune files per context names. *)
val gen :
     contexts:Context.t list
  -> ?only_packages:Package.t Package.Name.Map.t
  -> Dune_load.conf
  -> Super_context.t Context_name.Map.t Fiber.t

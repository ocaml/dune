(** Represent dependency graphs between OCaml modules *)

open Stdune

type t

val make
  :  dir:Path.Build.t
  -> per_module:((unit, Module.t list) Build.t) Module.Obj_map.t
  -> t

val deps_of
  :  t
  -> Module.t
  -> (unit, Module.t list) Build.t

val top_closed_implementations
  :  t
  -> Module.t list
  -> (unit, Module.t list) Build.t

module Ml_kind : sig
  type nonrec t = t Ml_kind.Dict.t

  val dummy : Module.t -> t

  val merge_for_impl : vlib:t -> impl:t -> t
end

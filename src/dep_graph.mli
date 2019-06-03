(** Represent dependency graphs between OCaml modules *)

open Stdune

type t

val make
  :  dir:Path.Build.t
  -> per_module:(Module.t * (unit, Module.t list) Build.t) Module.Name.Map.t
  -> t

val deps_of
  :  t
  -> Module.t
  -> (unit, Module.t list) Build.t

val top_closed_implementations
  :  t
  -> Module.t list
  -> (unit, Module.t list) Build.t

val top_closed_multi_implementations
  :  t list
  -> Module.t list
  -> (unit, Module.t list) Build.t


module Ml_kind : sig
  type nonrec t = t Ml_kind.Dict.t

  val dummy : Module.t -> t

  val wrapped_compat
    :  modules:Module.t Module.Name.Map.t
    -> wrapped_compat:Module.t Module.Name.Map.t
    -> t

  val merge_for_impl : vlib:t -> impl:t -> t
end

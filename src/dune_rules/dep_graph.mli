(** Represent dependency graphs between OCaml modules *)

open Import

type t

val make
  :  dir:Path.Build.t
  -> per_module:Module.t list Action_builder.t Module_name.Unique.Map.t
  -> t

val deps_of : t -> Module.t -> Module.t list Action_builder.t
val top_closed_implementations : t -> Module.t list -> Module.t list Action_builder.t

module Ml_kind : sig
  type nonrec t = t Ml_kind.Dict.t

  val dummy : Module.t -> t
end

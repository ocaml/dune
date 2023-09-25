(** V1 of the diagnostics module. *)

module Related : sig
  type t

  val to_diagnostic_related : t -> Exported_types.Diagnostic.Related.t
  val of_diagnostic_related : Exported_types.Diagnostic.Related.t -> t
end

type t

val sexp : (t, Conv.values) Conv.t
val to_diagnostic : t -> Exported_types.Diagnostic.t
val of_diagnostic : Exported_types.Diagnostic.t -> t

module Event : sig
  type t

  val sexp : (t, Conv.values) Conv.t
  val to_event : t -> Exported_types.Diagnostic.Event.t
  val of_event : Exported_types.Diagnostic.Event.t -> t
end

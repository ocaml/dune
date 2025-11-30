(** V2 of the diagnostics module. *)

type t

val sexp : (t, Conv.values) Conv.t
val to_diagnostic : t -> Exported_types.Diagnostic.t
val of_diagnostic : Exported_types.Diagnostic.t -> t

module Event : sig
  type t

  val sexp : t Conv.value
  val to_event : t -> Exported_types.Diagnostic.Event.t
  val of_event : Exported_types.Diagnostic.Event.t -> t
end

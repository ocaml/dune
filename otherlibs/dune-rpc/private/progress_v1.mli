(** V1 of Progress module. *)

type t =
  | Waiting
  | In_progress of
      { complete : int
      ; remaining : int
      }
  | Failed
  | Interrupted
  | Success

val sexp : (t, Conv.values) Conv.t
val to_progress : t -> Exported_types.Progress.t
val of_progress : Exported_types.Progress.t -> t

open Import

type verbosity =
  | Quiet  (** Only display errors *)
  | Short  (** One line per command *)
  | Verbose  (** Display all commands fully *)

type t =
  { status_line : bool
  ; verbosity : verbosity
  }

val all : (string * t) list

val to_dyn : t -> Dyn.t

(** The console backend corresponding to the selected display mode *)
val console_backend : t -> Console.Backend.t

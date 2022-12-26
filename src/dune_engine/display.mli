module Verbosity : sig
  type t =
    | Quiet  (** Only display errors *)
    | Short  (** One line per command *)
    | Verbose  (** Display all commands fully *)

  val to_dyn : t -> Dyn.t
end

type t =
  | Simple of
      { status_line : bool
      ; verbosity : Verbosity.t
      }
  | Tui

val all : (string * t) list

val to_dyn : t -> Dyn.t

(** The console backend corresponding to the selected display mode *)
val console_backend : t -> Dune_console.Backend.t

open! Stdune
(** Error reporting *)

(** Reports an error.

    Because dune doesn't stop at the first error, it might end up
    reporting the same error twice about missing libraries for
    instance. To avoid this, we keep a cache of reported errors and
    ignore errors that have already been reported.

    We cache what is actually printed to the screen.  *)
val report : Exn_with_backtrace.t -> unit

module Printer : sig
  type t

  (** If [backtrace] is [true], then any available backtrace will be
      printed. *)
  val make : ?backtrace:bool -> User_message.t -> t

  (** Register an error printer. *)
  val register : (exn -> t option) -> unit

  (** Find an error printer *)
  val find : exn -> t option

  val set_loc : t -> loc:Loc.t -> t
  val add_hint : t -> hint:User_message.Style.t Pp.t -> t
end


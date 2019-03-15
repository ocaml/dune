(** Exceptions *)

(** An programming error, that should be reported upstream. The error message
    should try to be developer friendly rather than user friendly. *)
exception Code_error of Sexp.t


(* CR-soon diml:
   - Rename to [User_error]
   - change the [string] argument to [Loc0.t option * string] and get rid of
   [Loc.Error]. The two are a bit confusing
   - change [string] to [Colors.Style.t Lib_name.t]
*)
(** A fatal error, that should be reported to the user in a nice way *)
exception Fatal_error of string

exception Loc_error of Loc0.t * string

val fatalf
  :  ?loc:Loc0.t
  -> ('a, unit, string, string, string, 'b) format6
  -> 'a

val code_error : string -> (string * Sexp.t) list -> _

type t = exn

external raise         : exn -> _ = "%raise"
external raise_notrace : exn -> _ = "%raise_notrace"
external reraise       : exn -> _ = "%reraise"

val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a
val protectx : 'a -> f:('a -> 'b) -> finally:('a -> unit) -> 'b

val pp_uncaught : backtrace:string -> Format.formatter -> t -> unit

val raise_with_backtrace: exn -> Printexc.raw_backtrace -> _

val equal : t -> t -> bool

val hash : t -> int

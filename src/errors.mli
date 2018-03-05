(** Dealing with errors *)

(* CR-soon diml: stop including this in [Import] *)
(** This module is included in [Import] *)

(* CR-soon diml:
   - Rename to [User_error]
   - change the [string] argument to [Loc.t option * string] and get rid of
   [Loc.Error]. The two are a bit confusing
   - change [string] to [Colors.Style.t Pp.t]
*)
(** A fatal error, that should be reported to the user in a nice way *)
exception Fatal_error of string

(* CR-soon diml: replace the [string] argument by [Usexp.t] *)
(** An programming error in the code of jbuilder, that should be reported upstream. The
    error message shouldn't try to be developper friendly rather than user friendly.  *)
exception Code_error of string

(* CR-soon diml: we won't need this once we can generate rules dynamically *)
(** Raised for errors that have already been reported to the user and shouldn't be
    reported again. This might happen when trying to build a dependency that has already
    failed. *)
exception Already_reported

(* CR-soon diml: Rename to [user_errorf]. *)
(** Raise a [Fatal_error] exception *)
val die : ('a, Format.formatter, unit, 'b) format4 -> 'a

(** Raise a [Code_error] exception *)
val code_errorf : ('a, Format.formatter, unit, 'b) format4 -> 'a

(**/**)
(* Referenced in Ansi_color and Report_error *)
val err_buf : Buffer.t
val err_ppf : Format.formatter
val kerrf
  :  ('a, Format.formatter, unit, 'b) format4
  -> f:(string -> 'b)
  -> 'a

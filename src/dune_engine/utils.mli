(** Utilities that can't go in [Import] *)

open Import

(** Raise an error about a program not found in the PATH or in the tree *)
val program_not_found
  :  ?context:Context_name.t
  -> ?hint:string
  -> loc:Loc.t option
  -> string
  -> _

val program_not_found_message
  :  ?context:Context_name.t
  -> ?hint:string
  -> loc:Loc.t option
  -> string
  -> User_message.t

(** Pretty-printer for suggesting a given shell command to the user *)
val pp_command_hint : string -> _ Pp.t

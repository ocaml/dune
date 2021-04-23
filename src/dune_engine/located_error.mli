(** A [Located_error.t] is morally a [User_error] annotated with an associated
    directory. Namely, if a build action raises an error, we want to preserve
    the directory that action was raised from. *)

open Stdune

exception E of User_message.t * Path.t

val raise :
     ?loc:Loc.t
  -> ?hints:User_message.Style.t Pp.t list
  -> dir:Path.t
  -> User_message.Style.t Pp.t list
  -> _

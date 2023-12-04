(** Non-fatal user errors *)

(** Warnings are user errors that cannot be proper errors for backward
    compatibility reasons *)

(** Emit a user warning. The arguments are interpreted in a similar fashion to
    {!User_error.raise} except that the first paragraph is prefixed with
    "Warning: " rather than "Error: ".

    If [is_error] is [true], then this function behaves exactly the same as
    [User_error.raise]. *)
val emit
  :  ?loc:Loc0.t
  -> ?hints:User_message.Style.t Pp.t list
  -> ?is_error:bool
  -> User_message.Style.t Pp.t list
  -> unit

(** [emit_message m] is like [emit], but allow you to provide the message as
    [m] instead of the arguments to construct the messagee. *)
val emit_message : User_message.t -> unit

(** Set the warning reporter. The default one is [User_message.prerr]. *)
val set_reporter : (User_message.t -> unit) -> unit

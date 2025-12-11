(** Module Bar referenced by Foo. *)

(** A type used by {!Foo}. *)
type bar_type = int

(** A function called from {!Foo.call_bar}. *)
val bar_function : bar_type -> bar_type

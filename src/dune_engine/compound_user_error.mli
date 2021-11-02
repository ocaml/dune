(** A compound user error defineds an alternative format for error messages that
    retains more structure. This can be used to display the errors in richer
    form by RPC clients. *)

open Stdune

type t = private
  { main : User_message.t
  ; related : User_message.t list
  }

include User_message.Annot.S with type payload := t

val make :
  main:User_message.t -> related:User_message.t list -> User_message.Annot.t

val parse_output : dir:Path.t -> string -> User_message.Annot.t option

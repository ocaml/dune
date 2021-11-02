(** A compound user error defineds an alternative format for error messages that
    retains more structure. This can be used to display the errors in richer
    form by RPC clients. *)

open Stdune

type t = private
  { main : User_message.t
  ; related : User_message.t list
  }

val annot : t User_message.Annots.Key.t

val make : main:User_message.t -> related:User_message.t list -> t

val parse_output : dir:Path.t -> string -> t option

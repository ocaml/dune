(** A compound user error defineds an alternative format for error messages that
    retains more structure. This can be used to display the errors in richer
    form by RPC clients. *)

open Import

type t = private
  { main : User_message.t
  ; related : User_message.t list
  }

val to_dyn : t -> Dyn.t
val annot : t list User_message.Annots.Key.t
val make : main:User_message.t -> related:User_message.t list -> t
val parse_output : rule_loc:Loc.t option -> dir:Path.t -> string -> t list

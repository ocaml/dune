(** Findlib database *)

exception Package_not_found of string

val query : pkg:string -> preds:string list -> var:string -> string option

(** Findlib database *)

exception Package_not_found of string

val root_packages : unit -> string list
val all_packages  : unit -> string list

val query : pkg:string -> preds:string list -> var:string -> string option

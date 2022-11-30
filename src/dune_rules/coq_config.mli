open Import

type t

val version :
  coqc:Action.Prog.t -> (string, User_message.Style.t Pp.t) Result.t Memo.t

val make : coqc:Action.Prog.t -> t Memo.t

val by_name :
  t -> string -> [> `Int of int | `Path of Path.t | `String of string ] option

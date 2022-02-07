type rebuild_trigger =
  | Eager
  | Passive

type t =
  | No
  | Yes of rebuild_trigger

val to_string : t -> string
val of_string : string -> (t, string) Result.t

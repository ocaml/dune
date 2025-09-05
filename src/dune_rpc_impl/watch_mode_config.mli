type rebuild_trigger =
  | Eager
  | Passive

type t =
  | No
  | Yes of rebuild_trigger

val equal : t -> t -> bool
val to_string : t -> string
val of_string : string -> (t, string) Result.t

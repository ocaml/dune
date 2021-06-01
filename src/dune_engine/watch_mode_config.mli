type rebuild_trigger =
  | Eager
  | Passive

type t =
  | No
  | Yes of rebuild_trigger

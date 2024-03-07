(** Safe version of Scanf from the stdlib. Does not raise parsing errors errors. *)

val unescaped : string -> (string, unit) Result.t

val sscanf
  :  string
  -> ('a, Stdlib.Scanf.Scanning.in_channel, 'b, 'c -> 'd, 'a -> 'e, 'e) format6
  -> 'c
  -> ('d, unit) result

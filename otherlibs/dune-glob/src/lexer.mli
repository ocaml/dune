open Stdune

type t =
  | Literal of string
  | Re of Dune_re.t

val parse_string : string -> (t, int * string) Result.result

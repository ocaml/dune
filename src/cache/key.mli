open Stdune

type t = Digest.t

val of_string : string -> (t, string) Result.t

val to_string : t -> string

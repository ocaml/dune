type bindings = string * string

type section =
  { name : string
  ; arg : string option
  ; bindings : bindings list
  }

type t = section list

val parse : string -> (t, string) result

type cmd = Fmt

val pkg_tools : string list
val pkg_of_binary : string -> string option
val pkg_of_cmd : cmd -> string * string option

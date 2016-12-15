open Import
val main : unit -> unit
val external_lib_deps : packages:string list -> String_set.t Path.Map.t
val report_error : ?map_fname:(string -> string) -> Format.formatter -> exn -> unit

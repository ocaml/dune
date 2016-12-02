val main : unit -> unit
val external_lib_deps : packages:string list -> string list
val report_error : ?map_fname:(string -> string) -> Format.formatter -> exn -> unit

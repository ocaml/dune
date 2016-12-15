open! Import
val main : unit -> unit
val external_lib_deps : packages:string list -> Build.lib_deps Path.Map.t
val report_error : ?map_fname:(string -> string) -> Format.formatter -> exn -> unit

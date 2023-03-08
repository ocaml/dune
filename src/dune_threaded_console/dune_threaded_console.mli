include module type of Dune_threaded_console_intf

val make : (module S) -> Dune_console.Backend.t

val progress : unit -> Dune_console.Backend.t

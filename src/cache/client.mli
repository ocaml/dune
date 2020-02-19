open Cache_intf
open Stdune

include Cache

val make :
     ?finally:(unit -> unit)
  -> ?duplication_mode:Duplication_mode.t
  -> (command -> unit)
  -> (t, exn) Result.t

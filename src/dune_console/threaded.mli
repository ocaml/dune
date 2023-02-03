open Stdune

val spawn_thread : ((unit -> unit) -> unit) Fdecl.t

val make : (module Threaded_intf.S) -> Backend_intf.t

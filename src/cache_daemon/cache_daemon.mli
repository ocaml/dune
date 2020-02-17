open Stdune
module Utils = Utils

type t

exception Error of string

type config =
  { exit_no_client : bool
  ; duplication_mode : Cache.Duplication_mode.t option
  }

val make : ?root:Path.t -> config:config -> unit -> t

val default_port_file : unit -> Path.t

val check_port_file :
     ?close:bool
  -> Path.t
  -> ((string * int * Unix.file_descr) option, exn) Result.t

val run : ?port_f:(string -> unit) -> ?port:int -> t -> unit

val stop : t -> unit

val endpoint : t -> string option

val daemon : root:Path.t -> config:config -> (string -> unit) -> unit

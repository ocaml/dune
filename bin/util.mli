open Stdune
open Dune

val parse_alias
  :  Path.t
  -> contexts:string list
  -> string list * Path.t * string

val check_path : Context.t list -> Path.t -> unit

val find_root : unit -> string * string list

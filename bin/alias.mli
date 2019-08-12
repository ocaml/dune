open Stdune

type t = private
  { name : string
  ; recursive : bool
  ; dir : Path.Source.t
  ; contexts : Dune.Context.t list
  }

val in_dir
  :  name:string
  -> recursive:bool
  -> contexts:Dune.Context.t list
  -> Path.t
  -> t

val of_string : Common.t -> recursive:bool -> string -> contexts:Dune.Context.t list -> t

val to_log_string : t -> string

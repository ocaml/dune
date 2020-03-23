open Stdune

type t = private
  { name : Dune.Alias.Name.t
  ; recursive : bool
  ; dir : Path.Source.t
  ; contexts : Dune.Context.t list
  }

val in_dir :
     name:Dune.Alias.Name.t
  -> recursive:bool
  -> contexts:Dune.Context.t list
  -> Path.t
  -> t

val of_string :
  Common.t -> recursive:bool -> string -> contexts:Dune.Context.t list -> t

val pp : t -> _ Pp.t

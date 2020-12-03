open Stdune

type t = private
  { name : Build_api.Api.Alias.Name.t
  ; recursive : bool
  ; dir : Path.Source.t
  ; contexts : Dune_rules.Context.t list
  }

val in_dir :
     name:Build_api.Api.Alias.Name.t
  -> recursive:bool
  -> contexts:Dune_rules.Context.t list
  -> Path.t
  -> t

val of_string :
     Common.t
  -> recursive:bool
  -> string
  -> contexts:Dune_rules.Context.t list
  -> t

val pp : t -> _ Pp.t

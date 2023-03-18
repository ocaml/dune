open Stdune
open Dune_engine

type t = private
  { name : Dune_engine.Alias.Name.t
  ; recursive : bool
  ; dir : Path.Source.t
  ; scontexts : Dune_rules.Super_context.t Context_name.Map.t
  }

val in_dir :
     name:Dune_engine.Alias.Name.t
  -> recursive:bool
  -> scontexts:Dune_rules.Super_context.t Context_name.Map.t
  -> Path.t
  -> t

val of_string :
     Workspace_root.t
  -> recursive:bool
  -> string
  -> scontexts:Dune_rules.Super_context.t Context_name.Map.t
  -> t

val pp : t -> _ Pp.t

val request : t -> unit Action_builder.t

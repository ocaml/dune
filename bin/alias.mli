open Import

type t = private
  { name : Dune_engine.Alias.Name.t
  ; recursive : bool
  ; dir : Path.Source.t
  ; contexts : Context.t list
  }

val in_dir
  :  name:Dune_engine.Alias.Name.t
  -> recursive:bool
  -> contexts:Context.t list
  -> Path.t
  -> t

val of_string
  :  Workspace_root.t
  -> recursive:bool
  -> string
  -> contexts:Context.t list
  -> t

val pp : t -> _ Pp.t
val request : t -> unit Action_builder.t

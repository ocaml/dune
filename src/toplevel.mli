open Stdune

module Source : sig
  type t

  val make
    :  dir:Path.Build.t
    -> loc:Loc.t
    -> main:string
    -> name:string
    -> t

  val loc : t -> Loc.t
  val modules : t -> Module.t Module.Name.Map.t
  val obj_dir : t -> Path.Build.t Obj_dir.t
end

type t

val setup_rules : t -> unit

val make : cctx:Compilation_context.t -> source:Source.t -> t

module Stanza : sig
  val setup
    :  sctx:Super_context.t
    -> dir:Path.Build.t
    -> toplevel:Dune_file.Toplevel.t
    -> unit
end

(** Utop rules *)

val utop_exe : Path.t -> Path.t
(** Return the path of the utop bytecode binary inside a directory where
    some libraries are defined. *)

val setup
  : Super_context.t
  -> dir:Path.t
  -> libs:Jbuild.Library.t list
  -> scope:Scope.t
  -> unit

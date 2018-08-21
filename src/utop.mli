(** Utop rules *)

open! Stdune

val utop_exe : Path.t -> Path.t
(** Return the path of the utop bytecode binary inside a directory where
    some libraries are defined. *)

val setup
  : Super_context.t
  -> dir:Path.t
  -> libs:Dune_file.Library.t list
  -> scope:Scope.t
  -> unit

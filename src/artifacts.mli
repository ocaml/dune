open! Import

type t

val create
  :  Context.t
  -> public_libs:Lib.DB.t
  -> 'a list
  -> f:('a -> Jbuild.Stanza.t list)
  -> t

(** A named artifact that is looked up in the PATH if not found in the tree

    If the name is an absolute path, it is used as it.
*)
val binary
  :  t
  -> ?hint:string
  -> string
  -> Action.Prog.t

(** [file_of_lib t ~from ~lib ~file] returns the path to a file in the
    directory of the given library. *)
val file_of_lib
  :  t
  -> loc:Loc.t
  -> lib:string
  -> file:string
  -> (Path.t, fail) result

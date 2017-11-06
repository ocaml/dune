open! Import

type t

val create
  :  Context.t
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

(** [file_of_lib t ~from name] a named artifact that is looked up in the given library.

    [name] is expected to be of the form "<lib>:<file>". Raises immediately if it is not
    the case. Returns "<lib>" as well as the resolved artifact.
*)
val file_of_lib
  :  t
  -> loc:Loc.t
  -> from:Path.t
  -> string
  -> string * (Path.t, fail) result

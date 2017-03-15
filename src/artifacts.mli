open! Import

type t

val create : Context.t -> (Path.t * Jbuild_types.Stanza.t list) list -> t

(** A named artifact that is looked up in the PATH if not found in the tree *)
val binary : t -> string -> (Path.t, fail) result

(** A named artifact that is looked up in the given library. *)
val file_of_lib
  :  ?use_provides:bool
  -> t
  -> from:Path.t
  -> lib:string
  -> file:string
  -> (Path.t, fail) result

(** [Named_artifact] provides a way to reference artifacts in jbuild rules without having
    to hardcode their exact locations. These named artifacts will be looked up
    appropriately (in the tree, or for the public release, possibly in the PATH or in
    findlib). *)

open! Import

type t

val create : Findlib.t -> (Path.t * Jbuild_types.Stanza.t list) list -> t

(** In the three following functions, the string argument matches the first argument of
    the [(provides ...)] stanza in the jbuild. *)

(** A named artifact that is looked up in the PATH if not found in the tree *)
val binary : t -> string -> Path.t

(** A named artifact that is looked up in the given findlib package if not found in the
    tree. Syntax is: ["<findlib_package>:<filename>"]. *)
val in_findlib : t -> string -> Path.t

(** OCaml flags *)

type t

val make : Jbuild.Buildable.t -> t

val default : unit -> t

val get : t -> Mode.t -> _ Arg_spec.t
val get_for_cm : t -> cm_kind:Cm_kind.t -> _ Arg_spec.t

val append_common : t -> string list -> t
val prepend_common : string list -> t -> t

val common : t -> string list

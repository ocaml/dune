(** OCaml flags *)

type t =
  { common   : string list
  ; specific : string list Mode.Dict.t
  }

val make : Jbuild_types.Buildable.t -> t

val default : unit -> t

val get : t -> Mode.t -> _ Arg_spec.t
val get_for_cm : t -> cm_kind:Cm_kind.t -> _ Arg_spec.t


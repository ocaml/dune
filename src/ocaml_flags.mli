(** OCaml flags *)

type t =
  { common   : string list Per_file.t
  ; specific : string list Per_file.t Mode.Dict.t
  }

val make : Jbuild_types.Buildable.t -> t

val default : unit -> t

val get : t -> Mode.t -> target:string -> _ Arg_spec.t
val get_for_cm : t -> target:string -> cm_kind:Cm_kind.t -> _ Arg_spec.t
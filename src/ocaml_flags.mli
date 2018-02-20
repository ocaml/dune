(** OCaml flags *)

type t

val make
  :  Jbuild.Buildable.t
  -> Super_context.t
  -> scope:Scope.t
  -> dir:Path.t
  -> t

val default : unit -> t

val empty : t

val get : t -> Mode.t -> (unit, string list) Build.t
val get_for_cm : t -> cm_kind:Cm_kind.t -> (unit, string list) Build.t

val append_common : t -> string list -> t
val prepend_common : string list -> t -> t

val common : t -> (unit, string list) Build.t

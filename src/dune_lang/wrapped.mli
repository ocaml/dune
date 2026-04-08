open Import

type t =
  | No
  | Yes of
      { transition : string option
      ; module_name : Module_name.t option
      }

val equal : t -> t -> bool

include Conv.S with type t := t

val to_bool : t -> bool
val to_dyn : t -> Dyn.t

(** Return the custom module name, if any. *)
val module_name : t -> Module_name.t option

(** Return the transition message, if any. *)
val transition : t -> string option

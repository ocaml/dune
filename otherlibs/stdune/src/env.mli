module Var : sig
  type t = string

  val compare : t -> t -> Ordering.t

  val temp_dir : t

  include Comparable_intf.S with type key := t
end

type t

val hash : t -> int

include Comparable_intf.S with type key := Var.t

val equal : t -> t -> bool

val empty : t

val vars : t -> Var.Set.t

(** The environment when the process started *)
val initial : t

val to_unix : t -> string list

val of_unix : string array -> t

val get : t -> Var.t -> string option

val extend : t -> vars:string Map.t -> t

val extend_env : t -> t -> t

val add : t -> var:Var.t -> value:string -> t

val remove : t -> var:Var.t -> t

val diff : t -> t -> t

val update : t -> var:Var.t -> f:(string option -> string option) -> t

val to_dyn : t -> Dyn.t

val of_string_map : string String.Map.t -> t

val to_map : t -> string Map.t

val iter : t -> f:(string -> string -> unit) -> unit

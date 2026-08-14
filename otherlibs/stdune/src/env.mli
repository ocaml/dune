(* Note that operations relating to the PATH environment variable are defined
   in a separate module [Env_path]. *)

module Var : sig
  type t

  val compare : t -> t -> Ordering.t

  include Comparable_intf.S with type key := t

  val of_string : string -> t
  val to_string : t -> string
  val repr : t Repr.t
  val temp_dir : t
  val _PATH : t
  val _OCAMLPARAM : t
  val _OCAMLFIND_CONF : t
  val _INSIDE_EMACS : t
  val _LC_ALL : t
  val _GIT_DIR : t
  val _XDG_CACHE_HOME : t
  val _DUNE_ACTION_TRACE_DIR : t
  val to_dyn : t -> Dyn.t
end

type t

val hash : t -> int

include Comparable_intf.S with type key := Var.t

val equal : t -> t -> bool
val empty : t
val is_empty : t -> bool
val vars : t -> Var.Set.t

(** The environment when the process started *)
val initial : t

val to_unix : t -> string list

(** Render the environment as a double-NUL-terminated Windows environment block. *)
val to_windows_block : t -> string

val of_unix : string array -> t
val get : t -> Var.t -> string option

(** [extend env ~vars] adds all variables from [vars] to [env] overwriting any
    existing values of those variables in [env] *)
val extend : t -> vars:string Map.t -> t

(** [extend_env a b] adds all variables from [b] to [a] overwriting any
    existing values of those variables in [a]. *)
val extend_env : t -> t -> t

val add : t -> var:Var.t -> value:string -> t
val mem : t -> var:Var.t -> bool
val remove : t -> var:Var.t -> t
val diff : t -> t -> t
val update : t -> var:Var.t -> f:(string option -> string option) -> t
val to_dyn : t -> Dyn.t
val of_string_map : string String.Map.t -> t
val to_map : t -> string Map.t
val of_map : string Map.t -> t
val iter : t -> f:(Var.t -> string -> unit) -> unit

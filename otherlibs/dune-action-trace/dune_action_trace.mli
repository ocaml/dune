(** Actions are allowed to produce events when executed by dune. Such events
    are aggregated into dune's trace file. *)

module Event : sig
  type t
  type args := (string * Csexp.t) list

  val instant
    :  ?args:args
    -> category:string
    -> name:string
    -> time_in_seconds:float
    -> unit
    -> t

  val span
    :  ?args:args
    -> category:string
    -> name:string
    -> start_in_seconds:float
    -> duration_in_seconds:float
    -> unit
    -> t
end

module Context : sig
  type t

  val is_enabled : t -> bool
  val create : name:string -> t
  val emit : t -> Event.t -> unit
  val close : t -> unit
end

module Private : sig
  val trace_dir_env_var : string
end

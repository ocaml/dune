module Event : sig
  type t

  val instant
    :  ?args:(string * Csexp.t) list
    -> category:string
    -> name:string
    -> time_in_seconds:float
    -> unit
    -> t

  val span
    :  ?args:(string * Csexp.t) list
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

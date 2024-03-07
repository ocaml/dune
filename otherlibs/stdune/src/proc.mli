val restore_cwd_and_execve : string -> string list -> env:Env.t -> _

module Resource_usage : sig
  type t =
    { user_cpu_time : float (** Same as the "user" time reported by the "time" command *)
    ; system_cpu_time : float (** Same as the "sys" time reported by the "time" command *)
    }
end

module Times : sig
  type t =
    { elapsed_time : float (** Same as the "real" time reported by the "time" command *)
    ; resource_usage : Resource_usage.t option
    }
end

module Process_info : sig
  type t =
    { pid : Pid.t
    ; status : Unix.process_status
    ; end_time : float (** Time at which the process finished. *)
    ; resource_usage : Resource_usage.t option
    }
end

type wait =
  | Any
  | Pid of Pid.t

(** This function is not implemented on Windows *)
val wait : wait -> Unix.wait_flag list -> Process_info.t

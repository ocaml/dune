(** [Proc.restore_cwd_and_execve prog args ~env] runs [prog] with [args] in [env].

  - [prog] is the program being run. It should be a filename in the current working
    directory.
  
  - [args] is a list of arguments to the program. Unlike in the system call [execve], the
    first argument is not the program name. The first argument is the first argument
    to the program. The program name is set to [prog] without the caller needing to.

  - [env] is the environment in which the program is run. *)
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

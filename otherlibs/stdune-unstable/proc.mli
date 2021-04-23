val restore_cwd_and_execve : string -> string list -> env:Env.t -> _

type resource_usage =
  { utime : float
  ; stime : float
  }

val wait3 : Unix.wait_flag list -> int * Unix.process_status * resource_usage

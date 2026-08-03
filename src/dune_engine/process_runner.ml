open Import

module Input = struct
  type t =
    | Null
    | Terminal
    | File of Path.t
end

module Output = struct
  type t =
    | Null
    | Terminal
    | File of
        { path : Path.t
        ; perm : Permissions.Mode.t
        }
end

module Stderr = struct
  type t =
    | Same_as_stdout
    | Output of Output.t
end

type request =
  { dir : Path.t option
  ; env : Env.t
  ; metadata : Process_metadata.t
  ; prog : Path.t
  ; args : string list
  ; stdin_from : Input.t
  ; stdout_to : Output.t
  ; stderr_to : Stderr.t
  ; create_process_group : bool
  ; timeout : Time.Span.t option
  ; queued : Time.Span.t
  }

type response =
  { started_at : Time.t
  ; process_info : Proc.Process_info.t
  ; termination_reason : Scheduler.termination_reason
  ; times : Proc.Times.t
  ; trace_args : (string * Sexp.t) list
  }

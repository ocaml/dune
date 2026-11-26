open Import

type stdio =
  | Null
  | Terminal
  | File of
      { path : Path.t
      ; perm : Permissions.Mode.t
      }
  | Fd of int

module Stderr : sig
  type t =
    | Same_as_stdout
    | Output of stdio
end

(** File redirections are either paths opened by the worker or indices into
    the descriptors transferred with the request. *)
type request =
  { dir : Path.t option
  ; env : Env.t
  ; metadata : Process_metadata.t
  ; prog : Path.t
  ; args : string Array.Immutable.t
  ; stdin_from : stdio
  ; stdout_to : stdio
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
    (** Extra fields to append to the parent-owned process trace events. *)
  }

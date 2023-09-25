(* [execve] doesn't exist on Windows, so instead we do a
   [Unix.create_process_env] followed by [Unix.waitpid] and finally [sys_exit].
   We use [sys_exit] rather than [exit] so that [at_exit] functions are not
   invoked. We don't want [at_exit] functions to be invoked to match the
   behaviour of [Unix.execve] on Unix. *)
external sys_exit : int -> 'a = "caml_sys_exit"

let restore_cwd_and_execve prog argv ~env =
  let env = Env.to_unix env |> Array.of_list in
  let argv = Array.of_list argv in
  (* run at_exit before changing the working directory *)
  Stdlib.do_at_exit ();
  Sys.chdir (Path.External.to_string Path.External.initial_cwd);
  if Sys.win32
  then (
    let pid = Unix.create_process_env prog argv env Unix.stdin Unix.stdout Unix.stderr in
    match snd (Unix.waitpid [] pid) with
    | WEXITED n -> sys_exit n
    | WSIGNALED _ -> sys_exit 255
    | WSTOPPED _ -> assert false)
  else (
    ignore (Unix.sigprocmask SIG_SETMASK [] : int list);
    Unix.execve prog argv env)
;;

module Resource_usage = struct
  type t =
    { user_cpu_time : float
    ; system_cpu_time : float
    }
end

module Times = struct
  type t =
    { elapsed_time : float
    ; resource_usage : Resource_usage.t option
    }
end

module Process_info = struct
  type t =
    { pid : Pid.t
    ; status : Unix.process_status
    ; end_time : float
    ; resource_usage : Resource_usage.t option
    }
end

external stub_wait4
  :  int
  -> Unix.wait_flag list
  -> int * Unix.process_status * float * Resource_usage.t
  = "dune_wait4"

type wait =
  | Any
  | Pid of Pid.t

let wait wait flags =
  if Sys.win32
  then Code_error.raise "wait4 not available on windows" []
  else (
    let pid =
      match wait with
      | Any -> -1
      | Pid pid -> Pid.to_int pid
    in
    let pid, status, end_time, resource_usage = stub_wait4 pid flags in
    { Process_info.pid = Pid.of_int pid
    ; status
    ; end_time
    ; resource_usage = Some resource_usage
    })
;;

(* [execve] doesn't exist on Windows, so instead we do a
   [Unix.create_process_env] followed by [Unix.waitpid] and finally [sys_exit].
   We use [sys_exit] rather than [exit] so that [at_exit] functions are not
   invoked. We don't want [at_exit] functions to be invoked to match the
   behaviour of [Unix.execve] on Unix. *)
external sys_exit : int -> 'a = "caml_sys_exit"

let restore_cwd_and_execve prog argv ~env =
  let env = Env.to_unix env |> Array.of_list in
  let argv = Array.of_list (prog :: argv) in
  (* run at_exit before changing the working directory *)
  Stdlib.do_at_exit ();
  Sys.chdir (Path.External.to_string Path.External.initial_cwd);
  if Sys.win32 || Platform.OS.value = Platform.OS.Haiku
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
    { user_cpu_time : Time.Span.t
    ; system_cpu_time : Time.Span.t
    ; maxrss : int
    ; minflt : int
    ; majflt : int
    ; inblock : int
    ; oublock : int
    ; nvcsw : int
    ; nivcsw : int
    }

  let zero =
    { user_cpu_time = Time.Span.zero
    ; system_cpu_time = Time.Span.zero
    ; maxrss = 0
    ; minflt = 0
    ; majflt = 0
    ; inblock = 0
    ; oublock = 0
    ; nvcsw = 0
    ; nivcsw = 0
    }
  ;;

  external get_self : unit -> t option = "dune_getrusage_self"
end

module Times = struct
  type t =
    { elapsed_time : Time.Span.t
    ; resource_usage : Resource_usage.t option
    }
end

module Process_info = struct
  type t =
    { pid : Pid.t
    ; status : Unix.process_status
    ; end_time : Time.t
    ; resource_usage : Resource_usage.t option
    }
end

module Linux = struct
  module Process_tree = struct
    type error =
      | Cannot_read_directory of
          { path : string
          ; error : Unix_error.Detailed.t
          }
      | Cannot_read_file of
          { path : string
          ; error : Unix_error.Detailed.t
          }
      | Cannot_parse_children_file of
          { path : string
          ; word : string
          }

    let pp_error = function
      | Cannot_read_directory { path; error } ->
        Pp.textf
          "unable to read process directory %s: %s"
          path
          (Unix_error.Detailed.to_string_hum error)
      | Cannot_read_file { path; error } ->
        Pp.textf
          "unable to read process children file %s: %s"
          path
          (Unix_error.Detailed.to_string_hum error)
      | Cannot_parse_children_file { path; word } ->
        Pp.textf "unable to parse process id %S in %s" word path
    ;;

    let parse_children_file path lines =
      Result.List.fold_left lines ~init:Pid.Set.empty ~f:(fun acc line ->
        String.split line ~on:' '
        |> Result.List.fold_left ~init:acc ~f:(fun acc word ->
          if String.is_empty word
          then Ok acc
          else (
            match Int.of_string word with
            | Some pid when pid > 0 -> Ok (Pid.Set.add acc (Pid.of_int_exn pid))
            | Some _ | None -> Error (Cannot_parse_children_file { path; word }))))
    ;;

    let lines_of_file path =
      Unix_error.Detailed.catch
        (fun path ->
           let fd = Unix.openfile path [ Unix.O_RDONLY ] 0 in
           let ic = Unix.in_channel_of_descr fd in
           Exn.protectx ic ~finally:close_in ~f:Io.input_lines)
        path
    ;;

    let children_of pid =
      let pid = Pid.to_int pid in
      match
        let tasks_dir = Printf.sprintf "/proc/%d/task" pid in
        match Readdir.read_directory tasks_dir with
        | Ok entries -> Ok entries
        | Error error -> Error (Cannot_read_directory { path = tasks_dir; error })
      with
      | Error _ as error -> error
      | Ok tasks ->
        Result.List.fold_left tasks ~init:Pid.Set.empty ~f:(fun acc tid ->
          let path = Printf.sprintf "/proc/%d/task/%s/children" pid tid in
          match lines_of_file path with
          | Error (Unix.ENOENT, _, _) -> Ok acc
          | Error error -> Error (Cannot_read_file { path; error })
          | Ok lines ->
            parse_children_file path lines |> Result.map ~f:(Pid.Set.union acc))
    ;;
  end
end

external stub_wait4
  :  int
  -> Unix.wait_flag list
  -> (int * Unix.process_status * int * Resource_usage.t) option
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
    stub_wait4 pid flags
    |> Option.map ~f:(fun (pid, status, end_time, resource_usage) ->
      { Process_info.pid = Pid.of_int_exn pid
      ; status
      ; end_time = Time.of_ns end_time
      ; resource_usage = Some resource_usage
      }))
;;

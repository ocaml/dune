open Import
open Fiber.O

type running_job =
  { pid  : int
  ; ivar : Unix.process_status Fiber.Ivar.t
  }

module Running_jobs : sig
  val add : running_job -> unit
  val wait : unit -> running_job * Unix.process_status
  val count : unit -> int
end = struct
  let all = Hashtbl.create 128

  let add job = Hashtbl.add all ~key:job.pid ~data:job

  let count () = Hashtbl.length all

  let resolve_and_remove_job pid =
    let job =
      Hashtbl.find_exn all pid ~string_of_key:(sprintf "<pid:%d>")
        ~table_desc:(fun _ -> "<running-jobs>")
    in
    Hashtbl.remove all pid;
    job

  exception Finished of running_job * Unix.process_status

  let wait_nonblocking_win32 () =
    match
      Hashtbl.iter all ~f:(fun ~key:pid ~data:job ->
        let pid, status = Unix.waitpid [WNOHANG] pid in
        if pid <> 0 then
          raise_notrace (Finished (job, status)))
    with
    | () -> None
    | exception (Finished (job, status)) ->
      Hashtbl.remove all job.pid;
      Some (job, status)

  let rec wait_win32 () =
    match wait_nonblocking_win32 () with
    | None ->
      ignore (Unix.select [] [] [] 0.001);
      wait_win32 ()
    | Some x -> x

  let wait_unix () =
    let pid, status = Unix.wait () in
    (resolve_and_remove_job pid, status)

  let wait =
    if Sys.win32 then
      wait_win32
    else
      wait_unix
end

type t =
  { log                       : Log.t
  ; original_cwd              : string
  ; display                   : Config.Display.t
  ; concurrency               : int
  ; waiting_for_available_job : t Fiber.Ivar.t Queue.t
  ; mutable status_line       : string
  ; mutable gen_status_line   : unit -> string option
  }

let log t = t.log
let display t = t.display

let with_chdir t ~dir ~f =
  Sys.chdir dir;
  protectx () ~finally:(fun () -> Sys.chdir t.original_cwd) ~f

let hide_status_line s ppf =
  let len = String.length s in
  if len > 0 then Format.fprintf ppf "\r%*s\r%!" len ""

let show_status_line s ppf =
  Format.pp_print_string ppf s;
  Format.pp_print_flush ppf ()

let print t fmt =
  let ppf = Format.err_formatter in
  let s = t.status_line in
  hide_status_line s ppf;
  Format.kfprintf (show_status_line s) ppf fmt

let t_var : t Fiber.Var.t = Fiber.Var.create ()

let set_status_line_generator f =
  Fiber.Var.get_exn t_var >>| fun t ->
  t.gen_status_line <- f

let wait_for_available_job () =
  Fiber.Var.get_exn t_var >>= fun t ->
  if Running_jobs.count () < t.concurrency then
    Fiber.return t
  else begin
    let ivar = Fiber.Ivar.create () in
    Queue.push ivar t.waiting_for_available_job;
    Fiber.Ivar.read ivar
  end

let wait_for_process pid =
  let ivar = Fiber.Ivar.create () in
  Running_jobs.add { pid; ivar };
  Fiber.Ivar.read ivar

let rec go_rec t =
  Fiber.yield ()
  >>= fun () ->
  let count = Running_jobs.count () in
  if count = 0 then begin
    Format.eprintf "%t%!" (hide_status_line t.status_line);
    Fiber.return ()
  end else begin
    if t.display = Progress then begin
      let ppf = Format.err_formatter in
      match t.gen_status_line () with
      | None ->
        if t.status_line <> "" then
          Format.eprintf "%t%!" (hide_status_line t.status_line)
      | Some status_line ->
        let status_line = sprintf "%s (jobs: %u)" status_line count in
        hide_status_line t.status_line ppf;
        show_status_line   status_line ppf;
        t.status_line <- status_line;
    end;
    let job, status = Running_jobs.wait () in
    (if not (Queue.is_empty t.waiting_for_available_job) then
       Fiber.Ivar.fill (Queue.pop t.waiting_for_available_job) t
     else
       Fiber.return ())
    >>= fun () ->
    Fiber.Ivar.fill job.ivar status
    >>= fun () ->
    go_rec t
  end

let go ?(log=Log.no_log) ?(config=Config.default)
      ?(gen_status_line=fun () -> None) fiber =
  Lazy.force Ansi_color.setup_env_for_colors;
  Log.info log ("Workspace root: " ^ !Clflags.workspace_root);
  let cwd = Sys.getcwd () in
  if cwd <> initial_cwd then
    Printf.eprintf "Entering directory '%s'\n%!" cwd;
  let t =
    { log
    ; gen_status_line
    ; original_cwd = cwd
    ; display      = config.display
    ; concurrency  = config.concurrency
    ; status_line  = ""
    ; waiting_for_available_job = Queue.create ()
    }
  in
  printer := { print = fun fmt -> print t fmt };
  let fiber =
    Fiber.Var.set t_var t
      (Fiber.with_error_handler (fun () -> fiber) ~on_error:Report_error.report)
  in
  Fiber.run
    (Fiber.fork_and_join_unit
       (fun () -> go_rec t)
       (fun () -> fiber))

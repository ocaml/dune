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

type info =
  { log : Log.t
  ; original_cwd : string
  }

let info_var : info Fiber.Var.t = Fiber.Var.create ()

let waiting_for_available_job = Queue.create ()
let wait_for_available_job () =
  if Running_jobs.count () < !Clflags.concurrency then
    Fiber.Var.get_exn info_var
  else begin
    let ivar = Fiber.Ivar.create () in
    Queue.push ivar waiting_for_available_job;
    Fiber.Ivar.read ivar
  end

let wait_for_process pid =
  let ivar = Fiber.Ivar.create () in
  Running_jobs.add { pid; ivar };
  Fiber.Ivar.read ivar

let rec go_rec info =
  Fiber.yield ()
  >>= fun () ->
  if Running_jobs.count () = 0 then
    Fiber.return ()
  else begin
    let job, status = Running_jobs.wait () in
    (if not (Queue.is_empty waiting_for_available_job) then
       Fiber.Ivar.fill (Queue.pop waiting_for_available_job) info
     else
       Fiber.return ())
    >>= fun () ->
    Fiber.Ivar.fill job.ivar status
    >>= fun () ->
    go_rec info
  end

let go ?(log=Log.no_log) fiber =
  Lazy.force Ansi_color.setup_env_for_colors;
  Log.info log ("Workspace root: " ^ !Clflags.workspace_root);
  let cwd = Sys.getcwd () in
  if cwd <> initial_cwd then
    Printf.eprintf "Entering directory '%s'\n%!" cwd;
  let info = { log; original_cwd = cwd } in
  let fiber =
    Fiber.Var.set info_var info
      (Fiber.with_error_handler (fun () -> fiber) ~on_error:Report_error.report)
  in
  Fiber.run
    (Fiber.fork (fun () -> go_rec info) >>= fun _ ->
     fiber)

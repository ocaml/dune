open! Stdune
open Import
open Fiber.O

type status_line_config =
  { message   : string option
  ; show_jobs : bool
  }

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

  let add job = Hashtbl.add all job.pid job

  let count () = Hashtbl.length all

  let resolve_and_remove_job pid =
    let job =
      match Hashtbl.find all pid with
      | Some job -> job
      | None -> assert false
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
  ; mutable concurrency       : int
  ; waiting_for_available_job : t Fiber.Ivar.t Queue.t
  ; mutable status_line       : string
  ; mutable gen_status_line   : unit -> status_line_config
  }

let log t = t.log
let display t = t.display

let with_chdir t ~dir ~f =
  Sys.chdir (Path.to_string dir);
  protectx () ~finally:(fun () -> Sys.chdir t.original_cwd) ~f

let hide_status_line s =
  let len = String.length s in
  if len > 0 then Printf.eprintf "\r%*s\r" len ""

let show_status_line s =
  prerr_string s

let print t msg =
  let s = t.status_line in
  hide_status_line s;
  prerr_string msg;
  show_status_line s;
  flush stderr

let t_var : t Fiber.Var.t = Fiber.Var.create ()

let set_status_line_generator f =
  Fiber.Var.get_exn t_var >>| fun t ->
  t.gen_status_line <- f

let set_concurrency n =
  Fiber.Var.get_exn t_var >>| fun t ->
  t.concurrency <- n

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

let rec restart_waiting_for_available_job t =
  if Queue.is_empty t.waiting_for_available_job ||
     Running_jobs.count () >= t.concurrency then
    Fiber.return ()
  else begin
    Fiber.Ivar.fill (Queue.pop t.waiting_for_available_job) t
    >>= fun () ->
    restart_waiting_for_available_job t
  end

let rec go_rec t =
  Fiber.yield ()
  >>= fun () ->
  let count = Running_jobs.count () in
  if count = 0 then begin
    hide_status_line t.status_line;
    flush stderr;
    Fiber.return ()
  end else begin
    if t.display = Progress then begin
      match t.gen_status_line () with
      | { message = None; _ } ->
        if t.status_line <> "" then begin
          hide_status_line t.status_line;
          flush stderr
        end
      | { message = Some status_line; show_jobs } ->
        let status_line =
          if show_jobs then
            sprintf "%s (jobs: %u)" status_line count
          else
            status_line
        in
        hide_status_line t.status_line;
        show_status_line   status_line;
        flush stderr;
        t.status_line <- status_line;
    end;
    let job, status = Running_jobs.wait () in
    Fiber.Ivar.fill job.ivar status
    >>= fun () ->
    restart_waiting_for_available_job t
    >>= fun () ->
    go_rec t
  end

let prepare ?(log=Log.no_log) ?(config=Config.default)
      ?(gen_status_line=fun () -> { message = None; show_jobs = false }) () =
  Log.infof log "Workspace root: %s"
    (Path.to_absolute_filename Path.root |> String.maybe_quoted);
  let cwd = Sys.getcwd () in
  if cwd <> initial_cwd then
    Printf.eprintf "Entering directory '%s'\n%!"
      (if Config.inside_dune then
         let descendant_simple p ~of_ =
           match
             String.drop_prefix p ~prefix:of_
           with
           | None | Some "" -> None
           | Some s -> Some (String.drop s 1)
         in
         match descendant_simple cwd ~of_:initial_cwd with
         | Some s -> s
         | None ->
           match descendant_simple initial_cwd ~of_:cwd with
           | None -> cwd
           | Some s ->
             let rec loop acc dir =
               if dir = Filename.current_dir_name then
                 acc
               else
                 loop (Filename.concat acc "..") (Filename.dirname dir)
             in
             loop ".." (Filename.dirname s)
       else
         cwd);
  let t =
    { log
    ; gen_status_line
    ; original_cwd = cwd
    ; display      = config.Config.display
    ; concurrency  = (match config.concurrency with Auto -> 1 | Fixed n -> n)
    ; status_line  = ""
    ; waiting_for_available_job = Queue.create ()
    }
  in
  Errors.printer := print t;
  t

let run t fiber =
  let fiber =
    Fiber.Var.set t_var t
      (Fiber.with_error_handler fiber ~on_error:Report_error.report)
  in
  Fiber.run
    (Fiber.fork_and_join_unit
       (fun () -> go_rec t)
       (fun () -> fiber))

let go ?log ?config ?gen_status_line fiber =
  let t = prepare ?log ?config ?gen_status_line () in
  run t (fun () -> fiber)

(** Fiber loop looks like this (if cache_init is true):
              /------------------\
              v                  |
    init --> once --> finally  --/

    The result of [~init] gets passed in every call to [~once] and [~finally].
    If cache_init is false, every iteration reexecutes init instead of
    saving it.

    [~watch] should return after the first change to any of the project files.
*)
let poll ?log ?config ?(cache_init=true) ~init ~once ~finally ~watch () =
  let t = prepare ?log ?config () in
  let wait_success () =
    let old_generator = t.gen_status_line in
    set_status_line_generator
      (fun () ->
         { message = Some "Success.\nWaiting for filesystem changes..."
         ; show_jobs = false
         })
    >>= fun () ->
    watch ()
    >>= fun _ ->
    set_status_line_generator old_generator
  in
  let wait_failure () =
    let old_generator = t.gen_status_line in
    set_status_line_generator
      (fun () ->
         { message = Some "Had errors.\nWaiting for filesystem changes..."
         ; show_jobs = false
         })
    >>= fun () ->
    (if Promotion.were_files_promoted () then
       Fiber.return ()
     else
       watch ())
    >>= fun _ ->
    set_status_line_generator old_generator
  in
  let rec main_loop () =
    (if cache_init then
       Fiber.return ()
     else
       init ())
    >>= fun _ ->
    once ()
    >>= fun _ ->
    finally ()
    >>= fun _ ->
    wait_success ()
    >>= fun _ ->
    main_loop ()
  in
  let continue_on_error () =
    finally ()
    >>= fun _ ->
    wait_failure ()
    >>= fun _ ->
    main_loop ()
  in
  let main () =
    (if cache_init then
       init ()
     else
       Fiber.return ())
    >>= fun _ ->
    main_loop ()
  in
  let rec loop f =
    try
      run t f
    with Fiber.Never ->
      loop continue_on_error
  in
  loop main

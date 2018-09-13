open! Stdune
open Import
open Fiber.O

type status_line_config =
  { message   : string option
  ; show_jobs : bool
  }

type running_job =
  { pid      : int
  ; build_id : int
  ; ivar     : Unix.process_status Fiber.Ivar.t
  }

let ignored_files_for_watch = String.Table.create 64

let ignore_for_watch path =
  assert (Path.is_in_source_tree path);
  String.Table.replace
    ignored_files_for_watch
    ~key:(Path.to_absolute_filename path)
    ~data:()

let don't_ignore_for_watch path =
  assert (Path.is_in_source_tree path);
  String.Table.remove ignored_files_for_watch (Path.to_string path)

module Watch : sig
  (** Runs forever, communicating events to channel when changes are detected. *)
  val wait_to_chan : unit Event.channel -> 'a

end = struct
  let command =
    lazy (
      let excludes = [ {|/_build|}
                     ; {|/\..+|}
                     ; {|~$|}
                     ; {|/#[^#]*#$|}
                     ; {|4913|} (* https://github.com/neovim/neovim/issues/3460 *)
                     ]
      in
      let path = Path.to_string_maybe_quoted Path.root in
      match Bin.which "inotifywait" with
      | Some inotifywait ->
        (* On Linux, use inotifywait. *)
        let excludes = String.concat ~sep:"|" excludes in
        inotifywait, [
          "-r"; path;
          "--exclude"; excludes;
          "-e"; "close_write"; "-e"; "delete";
          "--format"; "%w%f";
          "-m";
          "-q"]
      | None ->
        (* On all other platforms, try to use fswatch. fswatch's event
           filtering is not reliable (at least on Linux), so don't try to
           use it, instead act on all events. *)
        (match Bin.which "fswatch" with
         | Some fswatch ->
           let excludes = List.concat_map excludes ~f:(fun x -> ["--exclude"; x]) in
           fswatch, [
             "-r"; path;
             "--event"; "Created";
             "--event"; "Updated";
             "--event"; "Removed"] @ excludes
         | None ->
           die "@{<error>Error@}: fswatch (or inotifywait) was not found. \
                One of them needs to be installed for watch mode to work.\n"))

  let wait_to_chan chan =
    let prog, args = Lazy.force command in
    let prog = Path.to_absolute_filename prog in
    let args = Array.of_list (prog :: args) in
    let r, w = Unix.pipe () in
    let pid = Unix.create_process
                prog
                args
                (Unix.descr_of_in_channel stdin)
                w
                (Unix.descr_of_out_channel stderr)
    in
    let cleanup () =
      Unix.kill pid Sys.sigterm;
      ignore (Unix.waitpid [] pid)
    in
    at_exit cleanup;
    let r_in = Unix.in_channel_of_descr r in
    let rec loop () =
      let fn = Path.of_string (input_line r_in) in
      if not (String.Table.mem
                ignored_files_for_watch
                (Path.to_absolute_filename fn)) then
        Event.(sync (send chan ()));
      loop ()
    in
    loop ()
end

module Running_jobs : sig
  val add : running_job -> unit
  val count : unit -> int

  (** Runs forever, communicating events to channel when children terminate. *)
  val wait_to_chan : (int * Unix.process_status) Event.channel -> 'a

  (** Removes a process previously returned via channel from the table. *)
  val resolve_and_remove_job : int -> running_job

end = struct
  let all = Hashtbl.create 128

  (* Internal count is slightly different from logical count, since it counts
     the actual number of child processes, and so is decreased right after a
     process is waited on, while the logical count is decreased when a finished
     process is processed via [resolve_and_remove_job]. *)
  let i_count = ref 0
  let i_count_mtx = Mutex.create ()
  let i_count_cv = Condition.create ()

  let add job =
    Hashtbl.add all job.pid job;
    Mutex.lock i_count_mtx;
    i_count := !i_count + 1;
    if !i_count = 1 then
      Condition.signal i_count_cv;
    Mutex.unlock i_count_mtx

  let resolve_and_remove_job pid =
    let job =
      match Hashtbl.find all pid with
      | Some job -> job
      | None -> assert false
    in
    Hashtbl.remove all pid;
    job

  exception Finished of int * Unix.process_status

  let wait_nonblocking_win32 () =
    match
      Hashtbl.iter all ~f:(fun ~key:pid ~data:_ ->
        let pid, status = Unix.waitpid [WNOHANG] pid in
        if pid <> 0 then
          raise_notrace (Finished (pid, status)))
    with
    | () -> None
    | exception (Finished (pid, status)) ->
      Some (pid, status)

  let rec wait_win32 () =
    match wait_nonblocking_win32 () with
    | None ->
      ignore (Unix.select [] [] [] 0.001);
      wait_win32 ()
    | Some x -> x

  let wait_unix () =
    Unix.wait ()

  let wait =
    if Sys.win32 then
      wait_win32
    else
      wait_unix

  let rec wait_to_chan chan =
    if !i_count = 0 then
      Condition.wait i_count_cv i_count_mtx;
    Mutex.unlock i_count_mtx;
    let pid, status = wait () in
    Event.(sync (send chan (pid, status)));
    Mutex.lock i_count_mtx;
    i_count := !i_count - 1;
    wait_to_chan chan

  let wait_to_chan chan =
    Mutex.lock i_count_mtx;
    wait_to_chan chan

  let count () = Hashtbl.length all
end

type t =
  { log                        : Log.t
  ; original_cwd               : string
  ; display                    : Config.Display.t
  ; mutable concurrency        : int
  ; waiting_for_available_job  : waiting_job Queue.t
  ; mutable status_line        : string
  ; mutable gen_status_line    : unit -> status_line_config
  ; mutable cur_build_id       : int
  ; mutable cur_build_canceled : bool
  }

and waiting_job =
  { build_id : int
  ; ivar     : t Fiber.Ivar.t
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

let update_status_line t =
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
          sprintf "%s (jobs: %u)" status_line (Running_jobs.count ())
        else
          status_line
      in
      hide_status_line t.status_line;
      show_status_line   status_line;
      flush stderr;
      t.status_line <- status_line;
  end

let set_status_line_generator f =
  Fiber.Var.get_exn t_var >>| fun t ->
  t.gen_status_line <- f;
  update_status_line t

let set_concurrency n =
  Fiber.Var.get_exn t_var >>| fun t ->
  t.concurrency <- n

let wait_for_available_job () =
  Fiber.Var.get_exn t_var >>= fun t ->
  if Running_jobs.count () < t.concurrency then
    Fiber.return t
  else begin
    let ivar = Fiber.Ivar.create () in
    Queue.push { ivar; build_id = t.cur_build_id } t.waiting_for_available_job;
    Fiber.Ivar.read ivar
  end

let wait_for_process pid =
  Fiber.Var.get_exn t_var >>= fun t ->
  let ivar = Fiber.Ivar.create () in
  Running_jobs.add { pid; build_id = t.cur_build_id; ivar };
  Fiber.Ivar.read ivar

let rec restart_waiting_for_available_job t =
  if Queue.is_empty t.waiting_for_available_job ||
     Running_jobs.count () >= t.concurrency then
    Fiber.return ()
  else begin
    let { build_id; ivar } = Queue.pop t.waiting_for_available_job in
    begin
      if build_id = t.cur_build_id then
        Fiber.Ivar.fill ivar t
      else
        Fiber.return ()
    end
    >>= fun () ->
    restart_waiting_for_available_job t
  end

let watch_channel = lazy (
  if !Clflags.watch then begin
    let chan = Event.new_channel () in
    ignore (Thread.create Watch.wait_to_chan chan);
    chan
  end
  else Event.new_channel ())

let job_channel = lazy (
  let chan = Event.new_channel () in
  ignore (Thread.create Running_jobs.wait_to_chan chan);
  chan)

type event =
  | Files_changed
  | Job_completed of int * Unix.process_status

let go_rec t =
  let recv () = Event.(
    choose [ wrap (receive (Lazy.force watch_channel)) (fun () -> Files_changed)
           ; wrap (receive (Lazy.force job_channel)) (fun (p, s) -> Job_completed (p, s))
           ])
  in
  let rec go_rec t =
    Fiber.yield ()
    >>= fun () ->
    let count = Running_jobs.count () in
    if count = 0 then begin
      hide_status_line t.status_line;
      flush stderr;
      Fiber.return ()
    end else begin
      update_status_line t;
      begin
        match Event.sync (recv ()) with
        | Job_completed (pid, status) ->
          let job = Running_jobs.resolve_and_remove_job pid in
          begin
            if job.build_id = t.cur_build_id then
              Fiber.Ivar.fill job.ivar status
            else
              Fiber.return ()
          end
          >>= fun () ->
          restart_waiting_for_available_job t
          >>= fun () ->
          go_rec t
        | Files_changed ->
          t.cur_build_canceled <- true;
          Fiber.return ()
      end
    end
  in
  go_rec t

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
    ; cur_build_id = 0
    ; cur_build_canceled = false
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

let poll ?log ?config ~once ~finally ~canceled () =
  let watch_chan = Lazy.force watch_channel in
  let t = prepare ?log ?config () in
  let once () =
    t.cur_build_id <- t.cur_build_id + 1;
    t.cur_build_canceled <- false;
    once ()
  in
  let wait_success () =
    let old_generator = t.gen_status_line in
    set_status_line_generator
      (fun () ->
         { message = Some "Success.\nWaiting for filesystem changes..."
         ; show_jobs = false
         })
    >>= fun () ->
    Event.(sync (receive watch_chan));
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
    Event.(sync (receive watch_chan));
    set_status_line_generator old_generator
  in
  let rec main_loop () =
    once ()
    >>= fun _ ->
    finally ();
    wait_success ()
    >>= fun _ ->
    main_loop ()
  in
  let continue_on_error () =
    if not t.cur_build_canceled then begin
      finally ();
      wait_failure ()
      >>= fun _ ->
      main_loop ()
    end
    else begin
      canceled ();
      main_loop ()
    end
  in
  let rec loop f =
    try
      run t f
    with Fiber.Never ->
      loop continue_on_error
  in
  loop main_loop

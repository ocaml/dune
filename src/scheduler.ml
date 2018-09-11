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

module Watch : sig
  (** Has to be called before any other operations. *)
  val init : unit -> unit

  (** Runs forever, communicating () events to channel when changes are detected. *)
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
        inotifywait, ["-r"; path; "--exclude"; excludes; "-e"; "close_write"; "-e"; "delete"; "-m"; "-q"]
      | None ->
        (* On all other platforms, try to use fswatch. fswatch's event
           filtering is not reliable (at least on Linux), so don't try to
           use it, instead act on all events. *)
        (match Bin.which "fswatch" with
         | Some fswatch ->
           let excludes = List.concat_map excludes ~f:(fun x -> ["--exclude"; x]) in
           fswatch, ["-r"; path] @ excludes
         | None ->
           die "@{<error>Error@}: fswatch (or inotifywait) was not found. \
                One of them needs to be installed for watch mode to work.\n"))

  let pipe_r = ref None
  let pid = ref None

  let init () =
    let prog, args = Lazy.force command in
    let prog = Path.to_absolute_filename prog in
    let args = Array.of_list (prog :: args) in
    let r, w = Unix.pipe () in
    pid := Some (Unix.create_process
                   prog
                   args
                   (Unix.descr_of_in_channel stdin)
                   w
                   (Unix.descr_of_out_channel stderr));
    pipe_r := Some r;
    let cleanup () =
      match !pid with
      | Some pid ->
        Unix.kill pid Sys.sigterm;
        ignore (Unix.waitpid [] pid)
      | None -> ()
    in
    at_exit cleanup

  let buf_size = 65536
  let buf = Bytes.create buf_size

  let clear_pipe pipe_r =
    (* NOTE: this function is supposed to read and drop all data from the
       pipe in a non-blocking way, but due to Windows not supporting
       [set_nonblock], we settle for the possibility of multiple watch
       events being emitted if a large number of files changes at once. *)
    let bytes_read = Unix.read pipe_r buf 0 buf_size in
    if bytes_read = 0 then
      die "Watch pipe closed unexpectedly"

  let rec wait_to_chan chan =
    match !pipe_r with
    | Some r ->
      let forever = -1.0 in
      let _ = Unix.select [r] [] [] forever in
      clear_pipe r;
      Event.(sync (send chan ()));
      wait_to_chan chan
    | None ->
      die "Watch.write_to_chan() called without prior initialization"
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

  let add job = Hashtbl.add all job.pid job

  let count () = Hashtbl.length all

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
    try
      let pid, status = wait () in
      Event.(sync (send chan (pid, status)));
      wait_to_chan chan
    with
    | Unix.Unix_error (err, _, _) as exn ->
      if err = Unix.ECHILD then begin
        Thread.delay 0.001;
        wait_to_chan chan
      end
      else reraise exn

  let resolve_and_remove_job pid =
    let job =
      match Hashtbl.find all pid with
      | Some job -> job
      | None -> assert false
    in
    Hashtbl.remove all pid;
    job
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
    Watch.init ();
    let chan = Event.new_channel () in
    ignore (Thread.create Watch.wait_to_chan chan);
    chan
  end
  else Event.new_channel ())

let job_channel = lazy (
  let chan = Event.new_channel () in
  ignore (Thread.create Running_jobs.wait_to_chan chan);
  chan)

let go_rec t =
  let recv () = Event.(
    choose [ wrap (receive (Lazy.force watch_channel)) (fun () -> `Files_changed)
           ; wrap (receive (Lazy.force job_channel)) (fun x -> `Job_completed x)
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
        | `Job_completed (pid, status) ->
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
        | `Files_changed ->
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
    if not (Promotion.were_files_promoted ()) then
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

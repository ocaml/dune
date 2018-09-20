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

module Watch : sig
  (** Runs forever, communicating events to channel when changes are
      detected. *)
  val wait_to_chan : Path.t list Event.channel -> 'a

  (** Compares a batch of changed files to ignored files list.

      This should be only called from the main thread. *)
  val should_trigger_rebuild : Path.t list -> bool

  (** Ignore the next event about this file. *)
  val ignore_file : Path.t -> unit
end = struct
  let ignored_files = String.Table.create 64

  let ignore_file path =
    assert (Path.is_in_source_tree path);
    String.Table.replace
      ignored_files
      ~key:(Path.to_absolute_filename path)
      ~data:()

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
           let excludes =
             List.concat_map excludes ~f:(fun x -> ["--exclude"; x])
           in
           fswatch, [
             "-r"; path;
             "--event"; "Created";
             "--event"; "Updated";
             "--event"; "Removed"] @ excludes
         | None ->
           die "@{<error>Error@}: fswatch (or inotifywait) was not found. \
                One of them needs to be installed for watch mode to work.\n"))

  let buffering_time = 0.5 (* seconds *)

  let buffer_capacity = 65536
  let buffer = Bytes.create buffer_capacity
  let buffer_size = ref 0

  let read_lines fd =
    let len = Unix.read fd buffer !buffer_size (buffer_capacity - !buffer_size) in
    buffer_size := !buffer_size + len;
    let lines = ref [] in
    let line_start = ref 0 in
    for i = 0 to !buffer_size - 1 do
      let c = Bytes.get buffer i in
      if c = '\n' || c = '\r' then begin
        if !line_start < i then begin
          let line = Bytes.sub_string
                       buffer
                       ~pos:!line_start
                       ~len:(i - !line_start) in
          lines := line :: !lines;
        end;
        line_start := i + 1
      end
    done;
    buffer_size := !buffer_size - !line_start;
    Bytes.blit
      ~src:buffer ~src_pos:!line_start
      ~dst:buffer ~dst_pos:0
      ~len:!buffer_size;
    List.rev !lines

  let wait_to_chan chan =
    let new_events = ref [] in
    let event_mtx = Mutex.create () in
    let event_cv = Condition.create () in

    let worker_thread () =
      let prog, args = Lazy.force command in
      let prog = Path.to_absolute_filename prog in
      let args = Array.of_list (prog :: args) in
      let r, w = Unix.pipe () in
      let pid =
        Unix.create_process
          prog
          args
          Unix.stdin
          w
          Unix.stderr
      in
      let cleanup () =
        Unix.kill pid Sys.sigterm;
        ignore (Unix.waitpid [] pid)
      in
      at_exit cleanup;
      let rec loop () =
        let lines = List.map (read_lines r) ~f:Path.of_string in
        Mutex.lock event_mtx;
        new_events := lines :: !new_events;
        Condition.signal event_cv;
        Mutex.unlock event_mtx;
        loop ()
      in
      loop ()
    in

    let rec buffer_thread () =
      Mutex.lock event_mtx;
      if List.is_empty !new_events then
        Condition.wait event_cv event_mtx;
      let events_to_send = !new_events in
      new_events := [];
      Mutex.unlock event_mtx;
      List.iter events_to_send ~f:(fun batch ->
        Event.(sync (send chan batch)));
      Thread.delay buffering_time;
      buffer_thread ()
    in

    ignore (Thread.create worker_thread ());
    buffer_thread ()

  let should_trigger_rebuild fns =
    List.filter fns ~f:(fun fn ->
      let fn = Path.to_absolute_filename fn in
      if String.Table.mem ignored_files fn then begin
        (* only use ignored record once *)
        String.Table.remove ignored_files fn;
        false
      end
      else true)
    |> List.is_empty
    |> not
end

let ignore_for_watch = Watch.ignore_file

module Running_jobs : sig
  val add : running_job -> unit
  val count : unit -> int

  (** Runs forever, communicating events to channel when children terminate. *)
  val wait_to_chan : (int * Unix.process_status) Event.channel -> 'a

  (** Removes a process previously returned via channel from the table. *)
  val resolve_and_remove_job : int -> running_job

  (** Send the following signal to all running jobs *)
  val killall : int -> unit
end = struct
  let all = Hashtbl.create 128

  let running_pids = ref Int.Set.empty
  let running_pids_mtx = Mutex.create ()
  let something_is_running_cv = Condition.create ()

  let add job =
    Hashtbl.add all job.pid job;
    Mutex.lock running_pids_mtx;
    running_pids := Int.Set.add !running_pids job.pid;
    if Int.Set.cardinal !running_pids = 1 then
      Condition.signal something_is_running_cv;
    Mutex.unlock running_pids_mtx

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
    Mutex.lock running_pids_mtx;
    match
      Int.Set.iter !running_pids ~f:(fun pid ->
        let pid, status = Unix.waitpid [WNOHANG] pid in
        if pid <> 0 then
          raise_notrace (Finished (pid, status)));
    with
    | () ->
      Mutex.unlock running_pids_mtx;
      None
    | exception (Finished (pid, status)) ->
      Mutex.unlock running_pids_mtx;
      Some (pid, status)
    | exception exn ->
      Mutex.unlock running_pids_mtx;
      reraise exn

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
    if Int.Set.is_empty !running_pids then
      Condition.wait something_is_running_cv running_pids_mtx;
    Mutex.unlock running_pids_mtx;
    let pid, status = wait () in
    Event.(sync (send chan (pid, status)));
    Mutex.lock running_pids_mtx;
    running_pids := Int.Set.remove !running_pids pid;
    wait_to_chan chan

  let wait_to_chan chan =
    Mutex.lock running_pids_mtx;
    wait_to_chan chan

  let count () = Hashtbl.length all

  let killall signal =
    Hashtbl.iter all ~f:(fun ~key:pid ~data:_ -> Unix.kill pid signal)
end

type t =
  { log                        : Log.t
  ; original_cwd               : string
  ; display                    : Config.Display.t
  ; mutable concurrency        : int
  ; waiting_for_available_job  : t Fiber.Ivar.t Queue.t
  ; mutable status_line        : string
  ; mutable gen_status_line    : unit -> status_line_config
  ; mutable cur_build_canceled : bool
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
    let ivar = Queue.pop t.waiting_for_available_job in
    Fiber.Ivar.fill ivar t
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
  | Files_changed of Path.t list
  | Job_completed of int * Unix.process_status

let go_rec t =
  let recv () = Event.(
    choose [ wrap (receive (Lazy.force watch_channel))
               (fun fns -> Files_changed fns)
           ; wrap (receive (Lazy.force job_channel))
               (fun (p, s) -> Job_completed (p, s))
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
          Fiber.Ivar.fill job.ivar status
          >>= fun () ->
          restart_waiting_for_available_job t
          >>= fun () ->
          go_rec t
        | Files_changed fns ->
          if Watch.should_trigger_rebuild fns then begin
            t.cur_build_canceled <- true;
            Fiber.return ()
          end
          else go_rec t
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
    t.cur_build_canceled <- false;
    once ()
  in
  let rec block_waiting_for_changes () =
    let fns = Event.(sync (receive watch_chan)) in
    if Watch.should_trigger_rebuild fns then
      ()
    else
      block_waiting_for_changes ()
  in
  let wait_success () =
    let old_generator = t.gen_status_line in
    set_status_line_generator
      (fun () ->
         { message = Some "Success.\nWaiting for filesystem changes..."
         ; show_jobs = false
         })
    >>= fun () ->
    block_waiting_for_changes ();
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
    block_waiting_for_changes ();
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
      set_status_line_generator
        (fun () ->
           { message = Some "Had errors.\nKilling current build..."
           ; show_jobs = false
           })
      >>= fun () ->
      Queue.clear t.waiting_for_available_job;
      Running_jobs.killall Sys.sigkill;
      while Running_jobs.count () > 0 do
        let pid, _ = Event.sync (Event.receive (Lazy.force job_channel)) in
        ignore (Running_jobs.resolve_and_remove_job pid : running_job);
      done;
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

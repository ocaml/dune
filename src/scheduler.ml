open! Stdune
open Import
open Fiber.O

type job =
  { pid  : int
  ; ivar : Unix.process_status Fiber.Ivar.t
  }

(** The event queue *)
module Event : sig
  type t =
    | Files_changed
    | Job_completed of job * Unix.process_status

  (** Return the next event. File changes event are always flattened
      and returned first. *)
  val next : unit -> t

  (** Ignore the ne next file change event about this file. *)
  val ignore_next_file_change_event : Path.t -> unit

  (** Register the fact that a job was started. *)
  val register_job_started : unit -> unit

  (** Number of jobs for which the status hasn't been reported yet .*)
  val pending_jobs : unit -> int

  (** Send an event to the main thread. *)
  val send_files_changed : Path.t list -> unit
  val send_job_completed : job -> Unix.process_status -> unit
end = struct
  type t =
    | Files_changed
    | Job_completed of job * Unix.process_status

  let jobs_completed = Queue.create ()
  let files_changed = ref []
  let mutex = Mutex.create ()
  let cond = Condition.create ()

  let ignored_files = String.Table.create 64
  let pending_jobs = ref 0

  let register_job_started () = incr pending_jobs

  let ignore_next_file_change_event path =
    assert (Path.is_in_source_tree path);
    String.Table.replace
      ignored_files
      ~key:(Path.to_absolute_filename path)
      ~data:()

  let available () =
    not (List.is_empty !files_changed && Queue.is_empty jobs_completed)

  let next () =
    Mutex.lock mutex;
    let rec loop () =
      if not (available ()) then Condition.wait cond mutex;
      match !files_changed with
      | [] ->
        let (job, status) = Queue.pop jobs_completed in
        decr pending_jobs;
        Job_completed (job, status)
      | fns ->
        files_changed := [];
        let only_ignored_files =
          List.fold_left fns ~init:true ~f:(fun acc fn ->
            let fn = Path.to_absolute_filename fn in
            if String.Table.mem ignored_files fn then begin
              (* only use ignored record once *)
              String.Table.remove ignored_files fn;
              acc
            end else
              false)
        in
        if only_ignored_files then
          loop ()
        else
          Files_changed
    in
    let ev = loop () in
    Mutex.unlock mutex;
    ev

  let send_files_changed files =
    Mutex.lock mutex;
    let avail = available () in
    files_changed := List.rev_append files !files_changed;
    if not avail then Condition.signal cond;
    Mutex.unlock mutex

  let send_job_completed job status =
    Mutex.lock mutex;
    let avail = available () in
    Queue.push (job, status) jobs_completed;
    if not avail then Condition.signal cond;
    Mutex.unlock mutex

  let pending_jobs () = !pending_jobs
end

let ignore_for_watch = Event.ignore_next_file_change_event

type status_line_config =
  { message   : string option
  ; show_jobs : bool
  }

module File_watcher : sig
  (** Initializes the file watcher. *)
  val init : unit -> unit
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

  let init = lazy(
    let files_changed = ref [] in
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
        files_changed := List.rev_append lines !files_changed;
        Condition.signal event_cv;
        Mutex.unlock event_mtx;
        loop ()
      in
      loop ()
    in

    (* The buffer thread is used to avoid flooding the main thread
       with file changes events when a lot of file changes are reported
       at once. In particular, this avoids restarting the build over
       and over in a short period of time when many events are
       reported at once.

       It works as follow:

       - when the first event is received, send it to the main thread
       immediately so that we get a fast response time

       - after the first event is received, buffer subsequent events
       for [buffering_time]
    *)
    let rec buffer_thread () =
      Mutex.lock event_mtx;
      if List.is_empty !files_changed then
        Condition.wait event_cv event_mtx;
      let files = !files_changed in
      files_changed := [];
      Mutex.unlock event_mtx;
      Event.send_files_changed files;
      Thread.delay buffering_time;
      buffer_thread ()
    in

    ignore (Thread.create worker_thread () : Thread.t);
    ignore (Thread.create buffer_thread () : Thread.t))

  let init () = Lazy.force init
end

module Process_watcher : sig
  (** Initialize the process watcher thread. *)
  val init : unit -> unit

  (** Register a new running job. *)
  val register_job : job -> unit

  (** Send the following signal to all running processes. *)
  val killall : int -> unit
end = struct
  let mutex = Mutex.create ()
  let something_is_running_cv = Condition.create ()

  module Process_table : sig
    val add : job -> unit
    val remove : pid:int -> Unix.process_status -> unit
    val running_count : unit -> int
    val iter : f:(job -> unit) -> unit
  end = struct
    type process_state =
      | Running of job
      | Zombie of Unix.process_status

    (* Invariant: [!running_count] is equal to the number of
       [Running _] values in [table]. *)
    let table = Hashtbl.create 128
    let running_count = ref 0

    let add job =
      match Hashtbl.find table job.pid with
      | None ->
        Hashtbl.add table job.pid (Running job);
        incr running_count;
        if !running_count = 1 then Condition.signal something_is_running_cv
      | Some (Zombie status) ->
        Hashtbl.remove table job.pid;
        Event.send_job_completed job status
      | Some (Running _) ->
        assert false

    let remove ~pid status =
      match Hashtbl.find table pid with
      | None ->
        Hashtbl.add table pid (Zombie status)
      | Some (Running job) ->
        decr running_count;
        Hashtbl.remove table pid;
        Event.send_job_completed job status
      | Some (Zombie _) ->
        assert false

    let iter ~f =
      Hashtbl.iter table ~f:(fun ~key:_ ~data ->
        match data with
        | Running job -> f job
        | Zombie _ -> ())

    let running_count () = !running_count
  end

  let register_job job =
    Event.register_job_started ();
    Mutex.lock mutex;
    Process_table.add job;
    Mutex.unlock mutex

  let killall signal =
    Mutex.lock mutex;
    Process_table.iter ~f:(fun job ->
      try
        Unix.kill job.pid signal
      with Unix.Unix_error _ -> ());
    Mutex.unlock mutex

  exception Finished of job * Unix.process_status

  let wait_nonblocking_win32 () =
    try
      Process_table.iter ~f:(fun job ->
        let pid, status = Unix.waitpid [WNOHANG] job.pid in
        if pid <> 0 then
          raise_notrace (Finished (job, status)));
      false
    with Finished (job, status) ->
      (* We need to do the [Unix.waitpid] and remove the process while
         holding the lock, otherwise the pid might be reused in
         between. *)
      Process_table.remove ~pid:job.pid status;
      true

  let wait_win32 () =
    while not (wait_nonblocking_win32 ()) do
      Mutex.unlock mutex;
      ignore (Unix.select [] [] [] 0.001);
      Mutex.lock mutex
    done

  let wait_unix () =
    Mutex.unlock mutex;
    let pid, status = Unix.wait () in
    Mutex.lock mutex;
    Process_table.remove ~pid status

  let wait =
    if Sys.win32 then
      wait_win32
    else
      wait_unix

  let run () =
    Mutex.lock mutex;
    while true do
      if Process_table.running_count () = 0 then
        Condition.wait something_is_running_cv mutex;
      wait ()
    done

  let init = lazy (ignore (Thread.create run () : Thread.t))

  let init () = Lazy.force init
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
          sprintf "%s (jobs: %u)" status_line (Event.pending_jobs ())
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
  if Event.pending_jobs () < t.concurrency then
    Fiber.return t
  else begin
    let ivar = Fiber.Ivar.create () in
    Queue.push ivar t.waiting_for_available_job;
    Fiber.Ivar.read ivar
  end

let wait_for_process pid =
  let ivar = Fiber.Ivar.create () in
  Process_watcher.register_job { pid; ivar };
  Fiber.Ivar.read ivar

let rec restart_waiting_for_available_job t =
  if Queue.is_empty t.waiting_for_available_job ||
     Event.pending_jobs () >= t.concurrency then
    Fiber.return ()
  else begin
    let ivar = Queue.pop t.waiting_for_available_job in
    Fiber.Ivar.fill ivar t
    >>= fun () ->
    restart_waiting_for_available_job t
  end

let go_rec t =
  let rec go_rec t =
    Fiber.yield ()
    >>= fun () ->
    let count = Event.pending_jobs () in
    if count = 0 then begin
      hide_status_line t.status_line;
      flush stderr;
      Fiber.return ()
    end else begin
      update_status_line t;
      begin
        match Event.next () with
        | Job_completed (job, status) ->
          Fiber.Ivar.fill job.ivar status
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
  if !Clflags.watch then File_watcher.init ();
  Process_watcher.init ();
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
  let t = prepare ?log ?config () in
  let once () =
    t.cur_build_canceled <- false;
    once ()
  in
  let block_waiting_for_changes () =
    match Event.next () with
    | Job_completed _ -> assert false
    | Files_changed -> ()
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
    end else begin
      set_status_line_generator
        (fun () ->
           { message = Some "Had errors.\nKilling current build..."
           ; show_jobs = false
           })
      >>= fun () ->
      Queue.clear t.waiting_for_available_job;
      Process_watcher.killall Sys.sigkill;
      while Event.pending_jobs () > 0 do
        match Event.next () with
        | Files_changed -> ()
        | Job_completed _ -> ()
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

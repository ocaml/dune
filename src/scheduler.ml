open! Stdune
open Import
open Fiber.O

type job =
  { pid  : int
  ; ivar : Unix.process_status Fiber.Ivar.t
  }

module Signal = struct
  type t = Int | Quit | Term
  let compare : t -> t -> Ordering.t = compare

  module Set = Set.Make(struct
      type nonrec t = t
      let compare = compare
    end)

  let all = [Int; Quit; Term]

  let to_int = function
    | Int -> Sys.sigint
    | Quit -> Sys.sigquit
    | Term -> Sys.sigterm

  let _of_int =
    List.map all ~f:(fun t -> to_int t, t)
    |> Int.Map.of_list_reduce ~f:(fun _ t -> t)
    |> Int.Map.find

  let name t = Utils.signal_name (to_int t)
end

module Thread = struct end

type status_line_config =
  { message   : string option
  ; show_jobs : bool
  }

module File_watcher : sig
  type t

  (** Create a new file watcher. *)
  val create : unit -> t

  (** Pid of the external file watcher process *)
  val pid : t -> int

  (** Events the file watcher is interested in *)
  val events : t -> Ev_select.event list

  (** Reports the newly-changed file paths, given new events *)
  val files_changed : t -> Ev_select.event list -> Path.t list
end = struct
  let buffer_capacity = 65536

  type buffer =
    { data : Bytes.t
    ; mutable size : int
    }

  type t =
    { watcher_pid : int
    ; watcher_fd : Unix.file_descr
    ; buffer : buffer
    }

  let pid { watcher_pid; _ } = watcher_pid
  let events { watcher_fd; _ } = [ Ev_select.Ev_read_fd watcher_fd ]

  let command =
    lazy (
      let excludes = [ {|/_build|}
                     ; {|/_opam|}
                     ; {|/_esy|}
                     ; {|/\..+|}
                     ; {|~$|}
                     ; {|/#[^#]*#$|}
                     ; {|4913|} (* https://github.com/neovim/neovim/issues/3460 *)
                     ]
      in
      let path = Path.to_string_maybe_quoted Path.root in
      match Bin.which ~path:(Env.path Env.initial) "inotifywait" with
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
        (match Bin.which ~path:(Env.path Env.initial) "fswatch" with
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

  let read_lines buffer fd =
    let len =
      Unix.read fd buffer.data buffer.size (buffer_capacity - buffer.size)
    in
    buffer.size <- buffer.size + len;
    let lines = ref [] in
    let line_start = ref 0 in
    for i = 0 to buffer.size - 1 do
      let c = Bytes.get buffer.data i in
      if c = '\n' || c = '\r' then begin
        if !line_start < i then begin
          let line = Bytes.sub_string
                       buffer.data
                       ~pos:!line_start
                       ~len:(i - !line_start) in
          lines := line :: !lines;
        end;
        line_start := i + 1
      end
    done;
    buffer.size <- buffer.size - !line_start;
    Bytes.blit
      ~src:buffer.data ~src_pos:!line_start
      ~dst:buffer.data ~dst_pos:0
      ~len:buffer.size;
    List.rev_map !lines ~f:Path.of_string

  let files_changed { watcher_fd ; buffer ; _ } evs =
    if List.mem (Ev_select.Ev_read_fd watcher_fd) ~set:evs then
      read_lines buffer watcher_fd
    else
      []

  let spawn_external_watcher () =
    let prog, args = Lazy.force command in
    let prog = Path.to_absolute_filename prog in
    let argv = prog :: args in
    let r, w = Unix.pipe () in
    let pid =
      Spawn.spawn ()
        ~prog
        ~argv
        ~stdout:w
    in
    Unix.close w;
    (r, pid)

  let create () =
    let watcher_fd, watcher_pid = spawn_external_watcher () in
    let buffer =
      { data = Bytes.create buffer_capacity
      ; size = 0
      } in
    { watcher_fd; watcher_pid; buffer }

(*

FIXME

    let worker_thread pipe =
      let buffer =
        { data = Bytes.create buffer_capacity
        ; size = 0
        }
      in
      while true do
        let lines = read_lines buffer pipe in
        Mutex.lock event_mtx;
        files_changed := List.rev_append lines !files_changed;
        Condition.signal event_cv;
        Mutex.unlock event_mtx;
      done
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
    let buffering_time = 0.5 (* seconds *)

    let rec buffer_thread () =
      Mutex.lock event_mtx;
      while List.is_empty !files_changed do
        Condition.wait event_cv event_mtx
      done;
      let files = !files_changed in
      files_changed := [];
      Mutex.unlock event_mtx;
      Event.send_files_changed files;
      Thread.delay buffering_time;
      buffer_thread ()
    in

    ignore (Thread.create worker_thread pipe : Thread.t);
    ignore (Thread.create buffer_thread () : Thread.t);

    pid

*)

end

module Process_watcher : sig
  (** Register a new running job. *)
  val watch_job : job -> unit

  (** Send the following signal to all running processes. *)
  val killall : int -> unit

  (** Events the process watcher is interested in *)
  val events : unit -> Ev_select.event list

  (** Reports the newly-terminated processes, given new events *)
  val processes_terminated : Ev_select.event list -> (job * Unix.process_status) list
end = struct
  module Process_table : sig
    val add : job -> unit
    val remove : pid:int -> (job * Unix.process_status) option
    val iter : f:(job -> unit) -> unit
    val map : f:(job -> 'a) -> 'a list
  end = struct
    let table = Hashtbl.create 128

    let add job =
      match Hashtbl.find table job.pid with
      | None ->
        Hashtbl.add table job.pid job
      | Some _ ->
        assert false

    let remove ~pid =
      match Hashtbl.find table pid with
      | None ->
        None
      | Some job ->
        Hashtbl.remove table pid;
        Some (job, Ev_select.acknowledge_termination pid)

    let iter ~f =
      Hashtbl.iter table ~f:(fun ~key:_ ~data -> f data)

    let map ~f =
      Hashtbl.fold table ~init:[] ~f:(fun data acc -> f data :: acc)
  end

  let watch_job job =
    Process_table.add job

  let killall signal =
    Process_table.iter ~f:(fun job ->
      try
        Unix.kill job.pid signal
      with Unix.Unix_error _ -> ())

  let events () =
    Process_table.map ~f:(fun job ->
      Ev_select.Ev_child_process job.pid)

  let processes_terminated events =
    List.filter_map events ~f:(function
      | Ev_select.Ev_child_process pid -> Process_table.remove ~pid
      | _ -> None)
end

(*
module Signal_watcher : sig
  val init : unit -> unit
end = struct

  let signos = List.map Signal.all ~f:Signal.to_int

  let warning = {|

**************************************************************
* Press Control+C again quickly to perform an emergency exit *
**************************************************************

|}

  external sys_exit : int -> _ = "caml_sys_exit"

  let signal_waiter () =
    if Sys.win32 then begin
      let r, w = Unix.pipe () in
      let buf = Bytes.create 1 in
      Sys.set_signal Sys.sigint
        (Signal_handle (fun _ -> assert (Unix.write w buf 0 1 = 1)));
      Staged.stage (fun () ->
        assert (Unix.read r buf 0 1 = 1);
        Signal.Int)
    end else
      Staged.stage (fun () ->
        Thread.wait_signal signos
        |> Signal.of_int
        |> Option.value_exn)

  let run () =
    let last_exit_signals = Queue.create () in
    let wait_signal = Staged.unstage (signal_waiter ()) in
    while true do
      let signal = wait_signal () in
      Event.send_signal signal;
      match signal with
      | Int | Quit | Term ->
        let now = Unix.gettimeofday () in
        Queue.push now last_exit_signals;
        (* Discard old signals *)
        while Queue.length last_exit_signals >= 0 &&
              now -. Queue.peek last_exit_signals > 1.
        do
          ignore (Queue.pop last_exit_signals : float)
        done;
        let n = Queue.length last_exit_signals in
        if n = 2 then prerr_endline warning;
        if n = 3 then sys_exit 1
    done

  let init = lazy (ignore (Thread.create run () : Thread.t))

  let init () = Lazy.force init
end
*)

(** The event queue *)
module Event : sig
  type t =
    | Files_changed
    | Job_completed of job * Unix.process_status
    | Signal of Signal.t

  (** Return the next event. File changes event are always flattened
      and returned first. *)
  val next : ?file_watcher:File_watcher.t -> unit -> t

  (** Ignore the ne next file change event about this file. *)
  val ignore_next_file_change_event : Path.t -> unit

  (** Register a new job *)
  val register_job : job -> unit

  (** Number of jobs for which the status hasn't been reported yet .*)
  val pending_jobs : unit -> int
end = struct
  type t =
    | Files_changed
    | Job_completed of job * Unix.process_status
    | Signal of Signal.t

  let jobs_completed = Queue.create ()
  let files_changed = ref []
  let signals = ref Signal.Set.empty

  let ignored_files = String.Table.create 64
  let pending_jobs = ref 0

  let register_job job =
    incr pending_jobs;
    Process_watcher.watch_job job

  let ignore_next_file_change_event path =
    assert (Path.is_in_source_tree path);
    String.Table.replace
      ignored_files
      ~key:(Path.to_absolute_filename path)
      ~data:()

  let await_more_events ?file_watcher () =
    (* FIXME signals *)
    let events =
      Process_watcher.events ()
      @ (match file_watcher with
         | None -> []
         | Some fw -> File_watcher.events fw) in
    assert (events <> []);
    let events = Ev_select.select events in
    assert (events <> []);
    List.iter (Process_watcher.processes_terminated events) ~f:(fun p ->
        Queue.push p jobs_completed);
    (* FIXME signals *)
    (* FIXME file change debouncing *)
    begin match file_watcher with
    | None -> ()
    | Some fw ->
       let files = File_watcher.files_changed fw events in
       files_changed := List.rev_append files !files_changed
    end

  let next ?file_watcher () =
    Stats.record ();
    let rec loop () =
      match Signal.Set.choose !signals with
      | Some signal ->
        signals := Signal.Set.remove !signals signal;
        Signal signal
      | None ->
        match !files_changed with
        | [] ->
          if Queue.is_empty jobs_completed then
            (await_more_events ?file_watcher (); loop ())
          else
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
    loop ()

  let pending_jobs () = !pending_jobs
end


let ignore_for_watch = Event.ignore_next_file_change_event

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
  let t = Fiber.Var.get_exn t_var in
  t.gen_status_line <- f;
  update_status_line t

let set_concurrency n =
  let t = Fiber.Var.get_exn t_var in
  t.concurrency <- n

let wait_for_available_job () =
  let t = Fiber.Var.get_exn t_var in
  if Event.pending_jobs () < t.concurrency then
    Fiber.return t
  else begin
    let ivar = Fiber.Ivar.create () in
    Queue.push ivar t.waiting_for_available_job;
    Fiber.Ivar.read ivar
  end

let wait_for_process pid =
  let ivar = Fiber.Ivar.create () in
  Event.register_job { pid; ivar };
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

let got_signal t signal =
  if t.display = Verbose then
    Log.infof t.log "Got signal %s, exiting." (Signal.name signal)

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
        | Signal signal ->
          got_signal t signal;
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
  if cwd <> initial_cwd && not !Clflags.no_print_directory then
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

let run t f =
  let fiber =
    Fiber.Var.set t_var t
      (fun () -> Fiber.with_error_handler f ~on_error:Report_error.report)
  in
  Fiber.run
    (Fiber.fork_and_join_unit
       (fun () -> go_rec t)
       (fun () -> fiber))

let kill_and_wait_for_all_processes () =
  Process_watcher.killall Sys.sigkill;
  while Event.pending_jobs () > 0 do
    ignore (Event.next () : Event.t)
  done

let go ?log ?config ?gen_status_line f =
  let t = prepare ?log ?config ?gen_status_line () in
  try
    run t f
  with exn ->
    kill_and_wait_for_all_processes ();
    raise exn

type exit_or_continue = Exit | Continue
type got_signal = Got_signal

let poll ?log ?config ~once ~finally () =
  let t = prepare ?log ?config () in
  let watcher = File_watcher.create () in
  let once () =
    t.cur_build_canceled <- false;
    once ()
  in
  let block_waiting_for_changes () =
    match Event.next () with
    | Job_completed _ -> assert false
    | Files_changed -> Continue
    | Signal signal ->
      got_signal t signal;
      Exit
  in
  let rec wait msg =
    let old_generator = t.gen_status_line in
    set_status_line_generator
      (fun () ->
         { message = Some (msg ^ ".\nWaiting for filesystem changes...")
         ; show_jobs = false
         });
    let res = block_waiting_for_changes () in
    set_status_line_generator old_generator;
    match res with
    | Exit -> Fiber.return Got_signal
    | Continue -> main_loop ()
  and main_loop () =
    once ()
    >>= fun _ ->
    finally ();
    wait "Success"
  in
  let continue_on_error () =
    if not t.cur_build_canceled then begin
      finally ();
      wait "Had errors"
    end else begin
      set_status_line_generator
        (fun () ->
           { message = Some "Had errors.\nKilling current build..."
           ; show_jobs = false
           });
      Queue.clear t.waiting_for_available_job;
      Process_watcher.killall Sys.sigkill;
      let rec loop () =
        if Event.pending_jobs () = 0 then
          Continue
        else
          match Event.next () with
          | Files_changed -> loop ()
          | Job_completed _ -> loop ()
          | Signal signal ->
            got_signal t signal;
            Exit
      in
      match loop () with
      | Exit ->
        Fiber.return Got_signal
      | Continue ->
        finally ();
        main_loop ()
    end
  in
  let rec loop f =
    match run t f with
    | Got_signal -> (Fiber.Never, None)
    | exception Fiber.Never -> loop continue_on_error
    | exception exn -> (exn, Some (Printexc.get_raw_backtrace ()))
  in
  let exn, bt = loop main_loop in
  ignore (wait_for_process (File_watcher.pid watcher) : _ Fiber.t);
  kill_and_wait_for_all_processes ();
  match bt with
  | None -> Exn.raise exn
  | Some bt -> Exn.raise_with_backtrace exn bt

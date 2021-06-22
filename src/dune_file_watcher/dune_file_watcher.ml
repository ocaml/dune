open! Stdune
module Inotify_lib = Async_inotify_for_dune.Async_inotify

let inotify_event_paths (event : Inotify_lib.Event.t) =
  match event with
  | Created path
  | Unlinked path
  | Modified path
  | Moved (Away path)
  | Moved (Into path) ->
    [ path ]
  | Moved (Move (from, to_)) -> [ from; to_ ]
  | Queue_overflow -> []

type kind =
  | Coarse of { wait_for_watches_established : unit -> unit }
  | Fine of { inotify : Inotify_lib.t }

type t =
  { shutdown : [ `Kill of Pid.t | `No_op ]
  ; kind : kind
  }

module Event = struct
  type t =
    | File_changed of Path.t
    | Inotify_event of Inotify_lib.Event.t
    | Sync
    | Watcher_terminated
end

module Scheduler = struct
  type t =
    { spawn_thread : (unit -> unit) -> unit
    ; thread_safe_send_events : Event.t list -> unit
    ; thread_safe_send_job : (unit -> unit) -> unit
    }
end

let shutdown t = t.shutdown

let buffer_capacity = 65536

(* Fixed-size buffer for reading line-by-line from file descriptors. Bug:
   deadlocks if there's a line longer than the capacity of the buffer. TODO: use
   In_channel? *)
module Buffer = struct
  type buffer =
    { data : Bytes.t
    ; mutable size : int
    }

  let create ~capacity = { data = Bytes.create capacity; size = 0 }

  let read_lines buffer fd =
    let len =
      Unix.read fd buffer.data buffer.size (buffer_capacity - buffer.size)
    in
    buffer.size <- buffer.size + len;
    if len = 0 then
      `End_of_file (Bytes.sub_string buffer.data ~pos:0 ~len:buffer.size)
    else
      `Ok
        (let lines = ref [] in
         let line_start = ref 0 in
         for i = 0 to buffer.size - 1 do
           let c = Bytes.get buffer.data i in
           if c = '\n' || c = '\r' then (
             (if !line_start < i then
               let line =
                 Bytes.sub_string buffer.data ~pos:!line_start
                   ~len:(i - !line_start)
               in
               lines := line :: !lines);
             line_start := i + 1
           )
         done;
         buffer.size <- buffer.size - !line_start;
         Bytes.blit ~src:buffer.data ~src_pos:!line_start ~dst:buffer.data
           ~dst_pos:0 ~len:buffer.size;
         List.rev !lines)
end

module Inotifywait = struct
  let wait_for_watches_established stderr =
    let buffer = Buffer.create ~capacity:65536 in
    let rec loop () =
      match Buffer.read_lines buffer stderr with
      | `End_of_file _last_line -> `Error
      | `Ok lines ->
        if List.exists lines ~f:(String.equal "Watches established.") then
          `Established
        else
          loop ()
    in
    loop ()

  let parse_message s =
    match String.drop_prefix ~prefix:"e:" s with
    | None -> Error "invalid message (prefix missing)"
    | Some event -> (
      match String.lsplit2 ~on:':' event with
      | Some (_kind, path) -> Ok path
      | None -> Error "invalid message (event type missing)")
end

let special_file_for_inotify_sync =
  let path = lazy (Path.Build.relative Path.Build.root "dune-inotify-sync") in
  fun () -> Lazy.force path

let command ~root ~backend =
  let exclude_patterns =
    [ {|/_opam|}
    ; {|/_esy|}
    ; {|/\..+|}
    ; {|~$|}
    ; {|/#[^#]*#$|}
    ; {|4913|} (* https://github.com/neovim/neovim/issues/3460 *)
    ]
  in
  let exclude_paths =
    (* These paths should already exist on the filesystem when the watches are
       initially set up, otherwise the @<path> has no effect for inotifywait. If
       the file is deleted and re-created then "exclusion" is lost. This is why
       we're not including "_opam" and "_esy" in this list, in case they are
       created when dune is already running. *)
    (* these paths are used as patterns for fswatch, so they better not contain
       any regex-special characters *)
    [ "_build" ]
  in
  let root = Path.to_string root in
  let inotify_special_path =
    Path.Build.to_string (special_file_for_inotify_sync ())
  in
  match backend with
  | `Inotifywait inotifywait ->
    (* On Linux, use inotifywait. *)
    let excludes = String.concat ~sep:"|" exclude_patterns in
    ( inotifywait
    , List.concat
        [ [ "-r"; root ]
          (* excluding with "@" is more efficient that using --exclude because
             it avoids creating inotify watches altogether, while --exclude
             merely filters the events after they are generated *)
        ; List.map exclude_paths ~f:(fun path ->
              "@" ^ Filename.concat root path)
        ; [ inotify_special_path ]
        ; [ "--exclude"; excludes ]
        ; [ "-e"; "close_write" ]
        ; [ "-e"; "delete" ]
        ; [ "--format"; "e:%e:%w%f" ]
        ; [ "-m" ]
        ]
    , Inotifywait.parse_message
    , Some Inotifywait.wait_for_watches_established )
  | `Fswatch fswatch ->
    (* On all other platforms, try to use fswatch. fswatch's event filtering is
       not reliable (at least on Linux), so don't try to use it, instead act on
       all events. *)
    let excludes =
      List.concat_map
        (exclude_patterns @ List.map exclude_paths ~f:(fun p -> "/" ^ p))
        ~f:(fun x -> [ "--exclude"; x ])
    in
    ( fswatch
    , [ "-r"
      ; root
      ; (* If [inotify_special_path] is not passed here, then the [--exclude
           _build] makes fswatch not descend into [_build], which means it never
           even discovers that [inotify_special_path] exists. This is despite
           the fact that [--include] appears before. *)
        inotify_special_path
      ; "--event"
      ; "Created"
      ; "--event"
      ; "Updated"
      ; "--event"
      ; "Removed"
      ]
      @ [ "--include"; inotify_special_path ]
      @ excludes
    , (fun s -> Ok s)
    , None )

let select_watcher_backend ~use_inotify_lib =
  let try_fswatch () =
    Option.map
      (Bin.which ~path:(Env.path Env.initial) "fswatch")
      ~f:(fun fswatch -> `Fswatch fswatch)
  in
  let try_inotifywait () =
    Option.map
      (Bin.which ~path:(Env.path Env.initial) "inotifywait")
      ~f:(fun inotifywait -> `Inotifywait inotifywait)
  in
  let error str = User_error.raise [ Pp.text str ] in
  match Sys.linux with
  | false -> (
    match try_fswatch () with
    | Some res -> res
    | None -> error "Please install fswatch to enable watch mode.")
  | true -> (
    if use_inotify_lib then (
      assert (Ocaml_inotify.Inotify.supported_by_the_os ());
      `Inotify_lib
    ) else
      match try_inotifywait () with
      | Some res -> res
      | None -> (
        match try_fswatch () with
        | Some res -> res
        | None ->
          User_error.raise
            [ Pp.text
                "Please install inotifywait to enable watch mode. If \
                 inotifywait is unavailable, fswatch may also be used but will \
                 result in a worse experience."
            ]))

let emit_sync () =
  Io.write_file (Path.build (special_file_for_inotify_sync ())) "z"

let prepare_sync () =
  Path.mkdir_p (Path.parent_exn (Path.build (special_file_for_inotify_sync ())));
  emit_sync ()

let spawn_external_watcher ~root ~backend =
  prepare_sync ();
  let prog, args, parse_line, wait_for_start = command ~root ~backend in
  let prog = Path.to_absolute_filename prog in
  let argv = prog :: args in
  let r_stdout, w_stdout = Unix.pipe () in
  let stderr, wait =
    match wait_for_start with
    | None -> (None, fun () -> ())
    | Some wait -> (
      let r_stderr, w_stderr = Unix.pipe () in
      ( Some w_stderr
      , fun () ->
          match wait r_stderr with
          | `Error -> failwith "error waiting for watches to be established"
          | `Established -> () ))
  in
  let pid = Spawn.spawn () ~prog ~argv ~stdout:w_stdout ?stderr |> Pid.of_int in
  Unix.close w_stdout;
  Option.iter stderr ~f:Unix.close;
  ((r_stdout, parse_line, wait), pid)

let create_inotifylib_watcher ~(scheduler : Scheduler.t) =
  let special_file_for_inotify_sync = special_file_for_inotify_sync () in
  Inotify_lib.create ~spawn_thread:scheduler.spawn_thread
    ~modify_event_selector:`Closed_writable_fd
    ~send_emit_events_job_to_scheduler:(fun f ->
      scheduler.thread_safe_send_job (fun () ->
          let events = f () in
          let events =
            List.map events ~f:(fun event ->
                match (event : Inotify_lib.Event.t) with
                | Modified path
                  when Path.equal (Path.of_string path)
                         (Path.build special_file_for_inotify_sync) ->
                  Event.Sync
                | event -> Event.Inotify_event event)
          in
          (* this runs in the scheduler thread already, so we don't really need
             the "thread_safe_" part, but it doesn't hurt either *)
          scheduler.thread_safe_send_events events))
    ~log_error:(fun error -> Console.print [ Pp.text error ])

let create_no_buffering ~(scheduler : Scheduler.t) ~root ~backend =
  let special_file_for_inotify_sync = special_file_for_inotify_sync () in
  let (pipe, parse_line, wait), pid = spawn_external_watcher ~root ~backend in
  let worker_thread pipe =
    let buffer = Buffer.create ~capacity:buffer_capacity in
    let special_file_for_inotify_sync_absolute =
      Path.to_absolute_filename (Path.build special_file_for_inotify_sync)
    in
    while true do
      let lines =
        match Buffer.read_lines buffer pipe with
        | `End_of_file _remaining -> [ Event.Watcher_terminated ]
        | `Ok lines ->
          List.map lines ~f:(fun line ->
              match parse_line line with
              | Error s -> failwith s
              | Ok path_s -> (
                let path = Path.of_string path_s in
                match path with
                | In_source_tree _ -> Event.File_changed path
                | External _ ->
                  if String.equal path_s special_file_for_inotify_sync_absolute
                  then
                    Event.Sync
                  else
                    Event.File_changed path
                | In_build_dir build_path ->
                  if Path.Build.( = ) build_path special_file_for_inotify_sync
                  then
                    Event.Sync
                  else
                    Event.File_changed path))
      in
      scheduler.thread_safe_send_events lines
    done
  in
  scheduler.spawn_thread (fun () -> worker_thread pipe);
  { shutdown = `Kill pid
  ; kind = Coarse { wait_for_watches_established = wait }
  }

let with_buffering ~create ~(scheduler : Scheduler.t) ~debounce_interval =
  let files_changed = ref [] in
  let event_mtx = Mutex.create () in
  let event_cv = Condition.create () in
  let res =
    let thread_safe_send_events lines =
      Mutex.lock event_mtx;
      files_changed := List.rev_append lines !files_changed;
      Condition.signal event_cv;
      Mutex.unlock event_mtx
    in
    let scheduler = { scheduler with thread_safe_send_events } in
    create ~scheduler
  in
  (* The buffer thread is used to avoid flooding the main thread with file
     changes events when a lot of file changes are reported at once. In
     particular, this avoids restarting the build over and over in a short
     period of time when many events are reported at once.

     It works as follow:

     - when the first event is received, send it to the main thread immediately
     so that we get a fast response time

     - after the first event is received, buffer subsequent events for
     [debounce_interval] *)
  let rec buffer_thread () =
    Mutex.lock event_mtx;
    while List.is_empty !files_changed do
      Condition.wait event_cv event_mtx
    done;
    let files = !files_changed in
    files_changed := [];
    Mutex.unlock event_mtx;
    scheduler.thread_safe_send_events files;
    Thread.delay debounce_interval;
    buffer_thread ()
  in
  scheduler.spawn_thread buffer_thread;
  res

let create_external ~root ~debounce_interval ~scheduler ~backend =
  match debounce_interval with
  | None -> create_no_buffering ~root ~scheduler ~backend
  | Some debounce_interval ->
    with_buffering ~scheduler ~debounce_interval
      ~create:(create_no_buffering ~root)
      ~backend

let create_inotifylib ~scheduler =
  prepare_sync ();
  let inotify = create_inotifylib_watcher ~scheduler in
  Inotify_lib.add inotify
    (Path.to_string (Path.build (special_file_for_inotify_sync ())));
  { kind = Fine { inotify }; shutdown = `No_op }

let create_default ~scheduler =
  match select_watcher_backend ~use_inotify_lib:true with
  | (`Inotifywait _ | `Fswatch _) as backend ->
    create_external ~scheduler ~root:Path.root
      ~debounce_interval:(Some 0.5 (* seconds *)) ~backend
  | `Inotify_lib -> create_inotifylib ~scheduler

let create_external ~root ~debounce_interval ~scheduler =
  match select_watcher_backend ~use_inotify_lib:false with
  | (`Inotifywait _ | `Fswatch _) as backend ->
    create_external ~root ~debounce_interval ~scheduler ~backend
  | `Inotify_lib -> assert false

let wait_for_initial_watches_established_blocking t =
  match t.kind with
  | Coarse { wait_for_watches_established } -> wait_for_watches_established ()
  | Fine { inotify = _ } ->
    (* no initial watches needed: all watches should be set up at the time just
       before file access *)
    ()

let add_watch t path =
  match t.kind with
  | Coarse _ ->
    (* Here we assume that the path is already being watched because the coarse
       file watchers are expected to watch all the source files from the start *)
    ()
  | Fine { inotify } -> Inotify_lib.add inotify (Path.to_string path)

module For_tests = struct
  let pid t =
    match t.shutdown with
    | `Kill pid -> pid
    | `No_op -> failwith "don't know how to suspend an inotifylib watcher"

  let suspend t = Unix.kill (Pid.to_int (pid t)) Sys.sigstop

  let resume t = Unix.kill (Pid.to_int (pid t)) Sys.sigcont
end

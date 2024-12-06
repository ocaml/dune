open! Stdune
module Inotify_lib = Async_inotify_for_dune.Async_inotify
module Console = Dune_console

module Fs_memo_event = struct
  type kind =
    | Created
    | Deleted
    | File_changed
    | Unknown (** Treated conservatively as any possible event. *)

  let dyn_of_kind kind =
    Dyn.string
      (match kind with
       | Created -> "Created"
       | Deleted -> "Deleted"
       | File_changed -> "File_changed"
       | Unknown -> "Unknown")
  ;;

  type t =
    { path : Path.t
    ; kind : kind
    }

  let to_dyn { path; kind } =
    let open Dyn in
    record [ "path", Path.to_dyn path; "kind", dyn_of_kind kind ]
  ;;

  let create ~kind ~path =
    (match Path.as_in_build_dir path with
     | None -> ()
     | Some dir ->
       Code_error.raise
         "Fs_memo.Event.create called on a build path"
         [ "path", Path.Build.to_dyn dir ]);
    { path; kind }
  ;;
end

module Sync_id = Id.Make ()

module Event = struct
  type t =
    | Fs_memo_event of Fs_memo_event.t
    | Queue_overflow
    | Sync of Sync_id.t
    | Watcher_terminated
end

module Scheduler = struct
  type t =
    { spawn_thread : (unit -> unit) -> unit
    ; thread_safe_send_emit_events_job : (unit -> Event.t list) -> unit
    }
end

module Watch_trie : sig
  (** Specialized trie for fsevent watches *)
  type 'a t

  val empty : 'a t
  val to_list : 'a t -> (Path.External.t * 'a) list

  type 'a add =
    | Under_existing_node
    | Inserted of
        { new_t : 'a t
        ; removed : (Path.External.t * 'a) list
        }

  val add : 'a t -> Path.External.t -> 'a Lazy.t -> 'a add
end = struct
  (* the invariant is that a node can contain either a value or branches, but
     not both *)
  type 'a t =
    | Leaf of Path.External.t * 'a
    | Branch of 'a t String.Map.t

  type 'a add =
    | Under_existing_node
    | Inserted of
        { new_t : 'a t
        ; removed : (Path.External.t * 'a) list
        }

  let empty = Branch String.Map.empty

  let to_list t =
    let rec loop t acc =
      match t with
      | Leaf (k, v) -> (k, v) :: acc
      | Branch m -> String.Map.fold m ~init:acc ~f:loop
    in
    loop t []
  ;;

  let rec path p a = function
    | [] -> Leaf (p, a)
    | x :: xs -> Branch (String.Map.singleton x (path p a xs))
  ;;

  let add t key v =
    (* wrong in general, but this is only needed for fsevents *)
    let comps =
      match String.split ~on:'/' (Path.External.to_string key) with
      | "" :: comps -> comps
      | _ ->
        (* fsevents gives us only absolute paths *)
        assert false
    in
    let rec add comps t =
      match comps, t with
      | _, Leaf (_, _) -> Under_existing_node
      | [], Branch _ -> Inserted { new_t = Leaf (key, Lazy.force v); removed = to_list t }
      | x :: xs, Branch m ->
        (match String.Map.find m x with
         | None ->
           Inserted
             { new_t = Branch (String.Map.set m x (path key (Lazy.force v) xs))
             ; removed = []
             }
         | Some m' ->
           (match add xs m' with
            | Under_existing_node -> Under_existing_node
            | Inserted i ->
              Inserted { i with new_t = Branch (String.Map.set m x i.new_t) }))
    in
    add comps t
  ;;
end

type kind =
  | Fswatch of
      { pid : Pid.t
      ; wait_for_watches_established : unit -> unit
      }
  | Fsevents of
      { mutable external_ : Fsevents.t Watch_trie.t
      ; dispatch_queue : Fsevents.Dispatch_queue.t
      ; scheduler : Scheduler.t
      ; source : Fsevents.t
      ; sync : Fsevents.t
      ; latency : float
      ; on_event : Fsevents.Event.t -> Path.t -> Event.t option
      }
  | Inotify of Inotify_lib.t
  | Fswatch_win of
      { t : Fswatch_win.t
      ; scheduler : Scheduler.t
      }

type t =
  { kind : kind
  ; sync_table : (string, Sync_id.t) Table.t
    (* Pending fs sync operations indexed by the special sync filename. *)
  }

module Re = Dune_re

let create_should_exclude_predicate ~watch_exclusions =
  (* TODO we should really take the predicate directly and not depend on
     regular expressions in our file watching component *)
  Re.execp (Re.compile (Re.alt (List.map watch_exclusions ~f:Re.Posix.re)))
;;

module For_tests = struct
  let should_exclude = create_should_exclude_predicate
end

let process_inotify_event
      (event : Async_inotify_for_dune.Async_inotify.Event.t)
      should_exclude
  : Event.t list
  =
  let create_event_unless_excluded ~kind ~path =
    match should_exclude path with
    | true -> []
    | false ->
      let path = Path.of_string path in
      [ Event.Fs_memo_event (Fs_memo_event.create ~kind ~path) ]
  in
  match event with
  | Created path -> create_event_unless_excluded ~kind:Created ~path
  | Unlinked path -> create_event_unless_excluded ~kind:Deleted ~path
  | Modified path -> create_event_unless_excluded ~kind:File_changed ~path
  | Moved move ->
    (match move with
     | Away path -> create_event_unless_excluded ~kind:Deleted ~path
     | Into path -> create_event_unless_excluded ~kind:Created ~path
     | Move (from, to_) ->
       create_event_unless_excluded ~kind:Deleted ~path:from
       @ create_event_unless_excluded ~kind:Created ~path:to_)
  | Queue_overflow -> [ Queue_overflow ]
;;

let shutdown t =
  match t.kind with
  | Fswatch { pid; _ } -> `Kill pid
  | Inotify _ -> `No_op
  | Fsevents fsevents ->
    `Thunk
      (fun () ->
        Fsevents.stop fsevents.source;
        Fsevents.stop fsevents.sync;
        Watch_trie.to_list fsevents.external_
        |> List.iter ~f:(fun (_, fs) -> Fsevents.stop fs))
  | Fswatch_win { t; _ } -> `Thunk (fun () -> Fswatch_win.shutdown t)
;;

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
    let len = Unix.read fd buffer.data buffer.size (buffer_capacity - buffer.size) in
    buffer.size <- buffer.size + len;
    if len = 0
    then `End_of_file (Bytes.sub_string buffer.data ~pos:0 ~len:buffer.size)
    else
      `Ok
        (let lines = ref [] in
         let line_start = ref 0 in
         for i = 0 to buffer.size - 1 do
           let c = Bytes.get buffer.data i in
           if c = '\n' || c = '\r'
           then (
             if !line_start < i
             then (
               let line =
                 Bytes.sub_string buffer.data ~pos:!line_start ~len:(i - !line_start)
               in
               lines := line :: !lines);
             line_start := i + 1)
         done;
         buffer.size <- buffer.size - !line_start;
         Bytes.blit
           ~src:buffer.data
           ~src_pos:!line_start
           ~dst:buffer.data
           ~dst_pos:0
           ~len:buffer.size;
         List.rev !lines)
  ;;
end

module Fs_sync : sig
  val special_dir_path : Path.Build.t Lazy.t
  val special_dir : string Lazy.t
  val emit : t -> Sync_id.t
  val is_special_file : path_as_reported_by_file_watcher:string -> bool

  (** fsevents always reports absolute paths. therefore, we need callers to make
      an effort to determine if an absolute path is in fact in the build dir *)
  val is_special_file_fsevents : Path.t -> bool

  val consume_event : (string, 'a) Table.t -> string -> 'a option
end = struct
  let special_dir_path = lazy (Path.Build.relative Path.Build.root ".sync")
  let special_dir = lazy (Lazy.force special_dir_path |> Path.Build.to_string)

  let emit t =
    let id = Sync_id.gen () in
    let fn = id |> Sync_id.to_int |> string_of_int in
    let path = Filename.concat (Lazy.force special_dir) fn in
    Unix.close (Unix.openfile path [ O_WRONLY; O_CREAT; O_TRUNC ] 0o666);
    Table.set t.sync_table fn id;
    id
  ;;

  let is_special_file ~path_as_reported_by_file_watcher =
    (* We use string matching here and that's fine because we match on the path
       reported by the file watcher backend which will always report the same
       string that was used to setup the watch. *)
    Filename.dirname path_as_reported_by_file_watcher = Lazy.force special_dir
  ;;

  let consume_event table path =
    let basename = Filename.basename path in
    match Table.find table basename with
    | None -> None
    | Some id ->
      Fpath.unlink_no_err path;
      Table.remove table basename;
      Some id
  ;;

  let is_special_file_fsevents (path : Path.t) =
    match path with
    | In_source_tree _ | External _ -> false
    | In_build_dir build_path ->
      (match Path.Build.parent build_path with
       | None -> false
       | Some dir -> Path.Build.equal dir (Lazy.force special_dir_path))
  ;;
end

let command ~root ~backend ~watch_exclusions =
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
  let inotify_special_path = Lazy.force Fs_sync.special_dir in
  match backend with
  | `Fswatch fswatch ->
    (* On all other platforms, try to use fswatch. fswatch's event filtering is
       not reliable (at least on Linux), so don't try to use it, instead act on
       all events. *)
    let excludes =
      List.concat_map
        (watch_exclusions @ List.map exclude_paths ~f:(fun p -> "/" ^ p))
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
    , fun s -> Ok s )
;;

let fswatch_backend () =
  let try_fswatch () =
    Option.map
      (Bin.which ~path:(Env_path.path Env.initial) "fswatch")
      ~f:(fun fswatch -> `Fswatch fswatch)
  in
  match try_fswatch () with
  | Some res -> res
  | None ->
    let hints =
      match Platform.OS.value with
      | Haiku ->
        [ Pp.concat
            ~sep:Pp.space
            [ User_message.command "fswatch"; Pp.text "is available on HaikuPorts" ]
          |> Pp.hovbox
        ]
      | FreeBSD -> [ User_message.command "pkg install fswatch-mon" ]
      | _ -> []
    in
    User_error.raise
      ~hints
      [ Pp.concat
          ~sep:Pp.space
          [ Pp.text "Please install"
          ; User_message.command "fswatch"
          ; Pp.text "to enable watch mode."
          ]
        |> Pp.hovbox
      ]
;;

let select_watcher_backend () =
  if Fsevents.available ()
  then `Fsevents
  else if Ocaml_inotify.Inotify.supported_by_the_os ()
  then `Inotify_lib
  else (
    match Platform.OS.value with
    | Windows -> `Fswatch_win
    | Linux | Darwin | FreeBSD | OpenBSD | NetBSD | Haiku | Other -> fswatch_backend ())
;;

let prepare_sync () =
  let dir = Lazy.force Fs_sync.special_dir in
  match Fpath.clear_dir dir with
  | Cleared -> ()
  | Directory_does_not_exist ->
    (match Fpath.mkdir_p dir with
     | Already_exists | Created -> ())
;;

let spawn_external_watcher ~root ~backend ~watch_exclusions =
  prepare_sync ();
  let prog, args, parse_line = command ~root ~backend ~watch_exclusions in
  let prog = Path.to_absolute_filename prog in
  let argv = prog :: args in
  let r_stdout, w_stdout = Unix.pipe () in
  let stderr, wait = None, fun () -> () in
  let pid = Spawn.spawn () ~prog ~argv ~stdout:w_stdout ?stderr |> Pid.of_int in
  Unix.close w_stdout;
  Option.iter stderr ~f:Unix.close;
  (r_stdout, parse_line, wait), pid
;;

let create_inotifylib_watcher ~sync_table ~(scheduler : Scheduler.t) should_exclude =
  Inotify_lib.create
    ~spawn_thread:scheduler.spawn_thread
    ~modify_event_selector:`Closed_writable_fd
    ~send_emit_events_job_to_scheduler:(fun f ->
      scheduler.thread_safe_send_emit_events_job (fun () ->
        let events = f () in
        List.concat_map events ~f:(fun event ->
          let is_fs_sync_event_generated_by_dune =
            match (event : Inotify_lib.Event.t) with
            | Modified path | Created path | Unlinked path ->
              Option.some_if
                (Fs_sync.is_special_file ~path_as_reported_by_file_watcher:path)
                path
            | Moved _ | Queue_overflow -> None
          in
          match is_fs_sync_event_generated_by_dune with
          | None -> process_inotify_event event should_exclude
          | Some path ->
            (match Fs_sync.consume_event sync_table path with
             | None -> []
             | Some id -> [ Event.Sync id ]))))
    ~log_error:(fun error -> Console.print [ Pp.text error ])
;;

let create_no_buffering ~(scheduler : Scheduler.t) ~root ~backend ~watch_exclusions =
  let sync_table = Table.create (module String) 64 in
  let (pipe, parse_line, wait), pid =
    spawn_external_watcher ~root ~backend ~watch_exclusions
  in
  let worker_thread pipe =
    let buffer = Buffer.create ~capacity:buffer_capacity in
    while true do
      (* This job runs on the scheduler thread because it uses [sync_table]. *)
      let job =
        match Buffer.read_lines buffer pipe with
        | `End_of_file _remaining -> fun () -> [ Event.Watcher_terminated ]
        | `Ok lines ->
          fun () ->
            List.concat_map lines ~f:(fun line ->
              match parse_line line with
              | Error s -> failwith s
              | Ok path_s ->
                if Fs_sync.is_special_file ~path_as_reported_by_file_watcher:path_s
                then (
                  match Fs_sync.consume_event sync_table path_s with
                  | None -> []
                  | Some id -> [ Event.Sync id ])
                else (
                  let path = Path.Expert.try_localize_external (Path.of_string path_s) in
                  [ Fs_memo_event (Fs_memo_event.create ~kind:File_changed ~path) ]))
      in
      scheduler.thread_safe_send_emit_events_job job
    done
  in
  scheduler.spawn_thread (fun () -> worker_thread pipe);
  { kind = Fswatch { pid; wait_for_watches_established = wait }; sync_table }
;;

let with_buffering ~create ~(scheduler : Scheduler.t) ~debounce_interval =
  let jobs = ref [] in
  let event_mtx = Mutex.create () in
  let event_cv = Condition.create () in
  let res =
    let thread_safe_send_emit_events_job job =
      Mutex.lock event_mtx;
      jobs := job :: !jobs;
      Condition.signal event_cv;
      Mutex.unlock event_mtx
    in
    let scheduler = { scheduler with thread_safe_send_emit_events_job } in
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
    while List.is_empty !jobs do
      Condition.wait event_cv event_mtx
    done;
    let jobs_batch = List.rev !jobs in
    jobs := [];
    Mutex.unlock event_mtx;
    scheduler.thread_safe_send_emit_events_job (fun () ->
      List.concat_map jobs_batch ~f:(fun job -> job ()));
    Thread.delay debounce_interval;
    buffer_thread ()
  in
  scheduler.spawn_thread buffer_thread;
  res
;;

let create_inotifylib ~scheduler ~should_exclude =
  prepare_sync ();
  let sync_table = Table.create (module String) 64 in
  let inotify = create_inotifylib_watcher ~sync_table ~scheduler should_exclude in
  Inotify_lib.add inotify (Lazy.force Fs_sync.special_dir);
  { kind = Inotify inotify; sync_table }
;;

let fsevents_callback ?exclusion_paths (scheduler : Scheduler.t) ~f events =
  let skip_path =
    (* excluding a [path] will exclude children under [path] but not [path]
       itself. Hence we need to skip [path] manually *)
    match exclusion_paths with
    | None -> fun _ -> false
    | Some paths -> fun p -> List.mem paths p ~equal:Path.equal
  in
  scheduler.thread_safe_send_emit_events_job (fun () ->
    List.filter_map events ~f:(fun event ->
      let path =
        Fsevents.Event.path event |> Path.of_string |> Path.Expert.try_localize_external
      in
      if skip_path path then None else f event path))
;;

let fsevents ?exclusion_paths ~latency ~paths scheduler f =
  let paths = List.map paths ~f:Path.to_absolute_filename in
  let fsevents =
    Fsevents.create ~latency ~paths ~f:(fsevents_callback ?exclusion_paths scheduler ~f)
  in
  Option.iter exclusion_paths ~f:(fun paths ->
    let paths = List.rev_map paths ~f:Path.to_absolute_filename in
    Fsevents.set_exclusion_paths fsevents ~paths);
  fsevents
;;

let fsevents_standard_event ~should_exclude event path =
  if should_exclude (Path.to_string path)
  then None
  else (
    let kind =
      match Fsevents.Event.action event with
      | Rename | Unknown -> Fs_memo_event.Unknown
      | Create -> Created
      | Remove -> Deleted
      | Modify -> if Fsevents.Event.kind event = File then File_changed else Unknown
    in
    Some (Event.Fs_memo_event { Fs_memo_event.kind; path }))
;;

let create_fsevents ?(latency = 0.2) ~(scheduler : Scheduler.t) ~should_exclude () =
  prepare_sync ();
  let sync_table = Table.create (module String) 64 in
  let sync =
    (* We don't use the [fsevents] function here as we want to match on the
       original file path *)
    fsevents
      ~latency
      ~paths:[ Path.build (Lazy.force Fs_sync.special_dir_path) ]
      scheduler
      (fun event localized_path ->
         let path = Fsevents.Event.path event in
         if not (Fs_sync.is_special_file_fsevents localized_path)
         then None
         else (
           match Fsevents.Event.action event with
           | Remove -> None
           | Rename | Unknown | Create | Modify ->
             Option.map (Fs_sync.consume_event sync_table path) ~f:(fun id ->
               Event.Sync id)))
  in
  let on_event = fsevents_standard_event ~should_exclude in
  let source =
    let paths = [ Path.root ] in
    let exclusion_paths =
      Path.(build Build.root)
      :: ([ "_esy"; "_opam"; ".git"; ".hg" ]
          |> List.rev_map ~f:(Path.relative (Path.source Path.Source.root)))
    in
    fsevents ~latency scheduler ~exclusion_paths ~paths on_event
  in
  let cv = Condition.create () in
  let dispatch_queue_ref = ref None in
  let mutex = Mutex.create () in
  scheduler.spawn_thread (fun () ->
    let dispatch_queue = Fsevents.Dispatch_queue.create () in
    Mutex.lock mutex;
    dispatch_queue_ref := Some dispatch_queue;
    Condition.signal cv;
    Mutex.unlock mutex;
    Fsevents.start source dispatch_queue;
    Fsevents.start sync dispatch_queue;
    match Fsevents.Dispatch_queue.wait_until_stopped dispatch_queue with
    | Ok () -> ()
    | Error exn -> Code_error.raise "fsevents callback raised" [ "exn", Exn.to_dyn exn ]);
  let external_ = Watch_trie.empty in
  let dispatch_queue =
    Mutex.lock mutex;
    while !dispatch_queue_ref = None do
      Condition.wait cv mutex
    done;
    Mutex.unlock mutex;
    Option.value_exn !dispatch_queue_ref
  in
  { kind =
      Fsevents { latency; scheduler; sync; source; external_; dispatch_queue; on_event }
  ; sync_table
  }
;;

let fswatch_win_callback ~(scheduler : Scheduler.t) ~sync_table ~should_exclude event =
  let dir = Fswatch_win.Event.directory event in
  let filename = Filename.concat dir (Fswatch_win.Event.path event) in
  let localized_path = Path.Expert.try_localize_external (Path.of_string filename) in
  match localized_path with
  | In_build_dir _ ->
    if Fs_sync.is_special_file_fsevents localized_path
    then (
      match Fswatch_win.Event.action event with
      | Added | Modified ->
        (match Fs_sync.consume_event sync_table filename with
         | None -> ()
         | Some id -> scheduler.thread_safe_send_emit_events_job (fun () -> [ Sync id ]))
      | Removed | Renamed_new | Renamed_old -> ())
  | path ->
    let normalized_filename =
      String.concat
        ~sep:"/"
        (String.split_on_char ~sep:'\\' (String.lowercase_ascii filename))
    in
    if not (should_exclude normalized_filename)
    then
      scheduler.thread_safe_send_emit_events_job (fun () ->
        let kind =
          match Fswatch_win.Event.action event with
          | Added | Renamed_new -> Fs_memo_event.Created
          | Removed | Renamed_old -> Deleted
          | Modified -> File_changed
        in
        [ Fs_memo_event { kind; path } ])
;;

let create_fswatch_win ~(scheduler : Scheduler.t) ~debounce_interval:sleep ~should_exclude
  =
  let sync_table = Table.create (module String) 64 in
  let t = Fswatch_win.create () in
  Fswatch_win.add t (Path.to_absolute_filename Path.root);
  scheduler.spawn_thread (fun () ->
    while true do
      let events = Fswatch_win.wait t ~sleep in
      List.iter ~f:(fswatch_win_callback ~scheduler ~sync_table ~should_exclude) events
    done);
  { kind = Fswatch_win { t; scheduler }; sync_table }
;;

let create_external ~root ~debounce_interval ~scheduler ~backend =
  match debounce_interval with
  | None -> create_no_buffering ~root ~scheduler ~backend
  | Some debounce_interval ->
    with_buffering
      ~scheduler
      ~debounce_interval
      ~create:(create_no_buffering ~root)
      ~backend
;;

let create_default ?fsevents_debounce ~watch_exclusions ~scheduler () =
  let should_exclude = create_should_exclude_predicate ~watch_exclusions in
  match select_watcher_backend () with
  | `Fswatch _ as backend ->
    create_external
      ~scheduler
      ~root:Path.root
      ~debounce_interval:(Some 0.5 (* seconds *))
      ~backend
      ~watch_exclusions
  | `Fsevents -> create_fsevents ?latency:fsevents_debounce ~scheduler ~should_exclude ()
  | `Inotify_lib -> create_inotifylib ~scheduler ~should_exclude
  | `Fswatch_win ->
    create_fswatch_win
      ~scheduler
      ~should_exclude
      ~debounce_interval:500 (* milliseconds *)
;;

let wait_for_initial_watches_established_blocking t =
  match t.kind with
  | Fswatch c -> c.wait_for_watches_established ()
  | Fsevents _ | Inotify _ | Fswatch_win _ ->
    (* no initial watches needed: all watches should be set up at the time just
       before file access *)
    ()
;;

(* Return the parent directory of [ext] if [ext] denotes a file. *)
let parent_directory ext =
  let rec loop p =
    if Path.is_directory (Path.external_ p)
    then Some ext
    else (
      match Path.External.parent p with
      | None ->
        User_warning.emit
          [ Pp.textf "Refusing to watch %s" (Path.External.to_string ext) ];
        None
      | Some ext -> loop ext)
  in
  loop ext
;;

let add_watch t path =
  match t.kind with
  | Fsevents f ->
    (match path with
     | Path.In_source_tree _ -> (* already watched by source watcher *) Ok ()
     | In_build_dir _ -> Code_error.raise "attempted to watch a directory in build" []
     | External ext ->
       (match parent_directory ext with
        | None -> Ok ()
        | Some ext ->
          let watch =
            lazy (fsevents ~latency:f.latency f.scheduler ~paths:[ path ] f.on_event)
          in
          (match Watch_trie.add f.external_ ext watch with
           | Watch_trie.Under_existing_node -> Ok ()
           | Inserted { new_t; removed } ->
             let watch = Lazy.force watch in
             Fsevents.start watch f.dispatch_queue;
             List.iter removed ~f:(fun (_, fs) -> Fsevents.stop fs);
             f.external_ <- new_t;
             Ok ())))
  | Fswatch _ ->
    (* Here we assume that the path is already being watched because the coarse
       file watchers are expected to watch all the source files from the
       start *)
    Ok ()
  | Inotify inotify ->
    (try Ok (Inotify_lib.add inotify (Path.to_string path)) with
     | Unix.Unix_error (ENOENT, _, _) -> Error `Does_not_exist)
  | Fswatch_win fswatch ->
    (match path with
     | In_build_dir _ -> Code_error.raise "attempted to watch a directory in build" []
     | Path.In_source_tree _ -> Ok ()
     | External ext ->
       (match parent_directory ext with
        | None -> Ok ()
        | Some _ ->
          Fswatch_win.add fswatch.t (Path.to_absolute_filename path);
          Ok ()))
;;

let emit_sync = Fs_sync.emit

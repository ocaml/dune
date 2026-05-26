open Import
module Scheduler_event = Event
module Fs_memo_event = Scheduler_event.Fs_memo_event
module Sync_id = Scheduler_event.Sync_id
module Event = Scheduler_event.File_watcher_event

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
        Code_error.raise
          "Watch_trie.add called with a non-absolute path"
          [ "key", Path.External.to_dyn key ]
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
  | Fswatch of { pid : Pid.t }
  | Fsevents of
      { mutable external_ : Fsevents.t Watch_trie.t
      ; dispatch_queue : Fsevents.Dispatch_queue.t
      ; event_queue : Scheduler_event.Queue.t
      ; source : Fsevents.t
      ; sync : Fsevents.t
      ; latency : Time.Span.t
      ; on_event : Fsevents.Event.t -> Path.t -> Event.t option
      }
  | Inotify of Inotify.t
  | Fswatch_win of { t : Fswatch_win.t }

type sync_table =
  { table : (string, Sync_id.t) Table.t
  ; mutex : Mutex.t
  }

let create_sync_table () =
  { table = Table.create (module String) 64; mutex = Mutex.create () }
;;

type t =
  { kind : kind
  ; sync_table : sync_table
    (* Pending fs sync operations indexed by the special sync filename. *)
  }

let create_should_exclude_predicate ~watch_exclusions =
  (* TODO we should really take the predicate directly and not depend on
     regular expressions in our file watching component *)
  Re.execp (Re.compile (Re.alt (List.map watch_exclusions ~f:Re.Posix.re)))
;;

module For_tests = struct
  let should_exclude = create_should_exclude_predicate
end

let process_inotify_event (event : Inotify.Event.t) should_exclude : Event.t list =
  let create_event_unless_excluded ~kind ~path =
    if should_exclude path
    then []
    else (
      let path = Path.of_string path in
      [ Event.Fs_memo_event (Fs_memo_event.create ~kind ~path) ])
  in
  match event with
  | Queue_overflow -> [ Queue_overflow ]
  | Created path -> create_event_unless_excluded ~kind:Created ~path
  | Unlinked path -> create_event_unless_excluded ~kind:Deleted ~path
  | Modified path -> create_event_unless_excluded ~kind:File_changed ~path
  | Moved move ->
    (match move with
     | Away path -> create_event_unless_excluded ~kind:Deleted ~path
     | Into path -> create_event_unless_excluded ~kind:Created ~path
     | Move { src; dst } ->
       create_event_unless_excluded ~kind:Deleted ~path:src
       @ create_event_unless_excluded ~kind:Created ~path:dst)
;;

let trace_and_send_events send_events events =
  Dune_trace.emit_all ~buffered:true File_watcher (fun () ->
    List.map events ~f:(fun (event : Event.t) ->
      Dune_trace.Event.file_watcher
        (match event with
         | Queue_overflow -> `Queue_overflow
         | Sync sync -> `Sync (Sync_id.to_int sync)
         | Watcher_terminated -> `Watcher_terminated
         | Fs_memo_event { path; kind } ->
           let kind =
             match kind with
             | Created -> `Created
             | Deleted -> `Deleted
             | File_changed -> `File_changed
             | Unknown -> `Unknown
           in
           `File (path, kind))));
  send_events events
;;

let send_events event_queue events =
  trace_and_send_events
    (Scheduler_event.Queue.send_file_watcher_events event_queue)
    events
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
  | Fswatch_win { t } -> `Thunk (fun () -> Fswatch_win.shutdown t)
;;

module Fs_sync : sig
  val special_dir_path : Path.Build.t Lazy.t
  val special_dir : string Lazy.t
  val emit : sync_table -> Sync_id.t
  val is_special_file : path_as_reported_by_file_watcher:string -> bool

  (** fsevents always reports absolute paths. therefore, we need callers to make
      an effort to determine if an absolute path is in fact in the build dir *)
  val is_special_file_fsevents : Path.t -> bool

  val consume_event : sync_table -> string -> Sync_id.t option
end = struct
  let special_dir_path = lazy (Path.Build.relative Path.Build.root ".sync")
  let special_dir = lazy (Lazy.force special_dir_path |> Path.Build.to_string)

  let emit sync_table =
    let id = Sync_id.gen () in
    let fn = id |> Sync_id.to_int |> string_of_int in
    let path = Filename.concat (Lazy.force special_dir) fn in
    Mutex.protect sync_table.mutex (fun () ->
      Table.set sync_table.table fn id;
      match
        Unix.close (Unix.openfile path [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o666)
      with
      | () -> ()
      | exception exn ->
        Table.remove sync_table.table fn;
        Exn.reraise exn);
    id
  ;;

  let is_special_file ~path_as_reported_by_file_watcher =
    (* We use string matching here and that's fine because we match on the path
       reported by the file watcher backend which will always report the same
       string that was used to setup the watch. *)
    Filename.dirname path_as_reported_by_file_watcher = Lazy.force special_dir
  ;;

  let consume_event sync_table path =
    Mutex.protect sync_table.mutex (fun () ->
      let basename = Filename.basename path in
      match Table.find sync_table.table basename with
      | None -> None
      | Some id ->
        Fpath.unlink_no_err path;
        Table.remove sync_table.table basename;
        Some id)
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

let command ~backend ~watch_exclusions =
  let inotify_special_path = Lazy.force Fs_sync.special_dir in
  match backend with
  | `Fswatch fswatch ->
    (* On all other platforms, try to use fswatch. fswatch's event filtering is
       not reliable (at least on Linux), so don't try to use it, instead act on
       all events. *)
    let excludes =
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
      watch_exclusions @ List.map exclude_paths ~f:(fun p -> "/" ^ p)
      |> List.concat_map ~f:(fun x -> [ "--exclude"; x ])
    in
    ( fswatch
    , [ "-r"
      ; Path.to_string Path.root
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
      @ excludes )
;;

let fswatch_backend () =
  match
    Bin.which ~path:(Env_path.path Env.initial) "fswatch"
    |> Option.map ~f:(fun fswatch -> `Fswatch fswatch)
  with
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
     | `Already_exists | `Created -> ())
;;

let spawn_external_watcher ~backend ~watch_exclusions =
  prepare_sync ();
  let prog, args = command ~backend ~watch_exclusions in
  let r_stdout, w_stdout = Unix.pipe () in
  let pid =
    let prog = Path.to_absolute_filename prog in
    let argv = prog :: args in
    (* CR-someday rgrinberg: we sohuldn't let this program write anything to our stderr *)
    Spawn.spawn () ~prog ~argv ~stdout:w_stdout |> Pid.of_int
  in
  Unix.close w_stdout;
  Unix.in_channel_of_descr r_stdout, pid
;;

let create_inotifylib_watcher ~sync_table ~event_queue should_exclude =
  Inotify.create
    ~mutex:sync_table.mutex
    ~modify_event_selector:`Closed_writable_fd
    ~emit_events:(fun events ->
      let events =
        List.concat_map events ~f:(fun event ->
          let is_fs_sync_event_generated_by_dune =
            match (event : Inotify.Event.t) with
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
             | Some id -> [ Event.Sync id ]))
      in
      send_events event_queue events)
;;

let create_external_fswatch ~event_queue ~backend ~watch_exclusions =
  let debounce_interval = Time.Span.of_secs 0.5 in
  let jobs = ref [] in
  let event_mtx = Mutex.create () in
  let event_cv = Condition.create () in
  let res =
    let sync_table = create_sync_table () in
    let pipe, pid = spawn_external_watcher ~backend ~watch_exclusions in
    let (_ : Thread.t) =
      Thread0.spawn ~name:"file-watcher" (fun () ->
        let enqueue events =
          Mutex.protect event_mtx (fun () ->
            jobs := events :: !jobs;
            Condition.signal event_cv)
        in
        let rec loop () =
          match input_line pipe with
          | exception End_of_file -> enqueue [ Event.Watcher_terminated ]
          | path_s ->
            let events =
              if Fs_sync.is_special_file ~path_as_reported_by_file_watcher:path_s
              then (
                match Fs_sync.consume_event sync_table path_s with
                | None -> []
                | Some id -> [ Event.Sync id ])
              else (
                let path = Path.Expert.try_localize_external (Path.of_string path_s) in
                [ Fs_memo_event (Fs_memo_event.create ~kind:File_changed ~path) ])
            in
            enqueue events;
            loop ()
        in
        loop ())
    in
    { kind = Fswatch { pid }; sync_table }
  in
  let (_ : Thread.t) =
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
      let jobs_batch =
        Mutex.protect event_mtx (fun () ->
          while List.is_empty !jobs do
            Condition.wait event_cv event_mtx
          done;
          let jobs_batch = List.rev !jobs in
          jobs := [];
          jobs_batch)
      in
      send_events event_queue (List.concat jobs_batch);
      Thread.delay (Time.Span.to_secs debounce_interval);
      buffer_thread ()
    in
    Thread0.spawn ~name:"file-watcher-buffer" buffer_thread
  in
  res
;;

let create_inotifylib ~event_queue ~should_exclude =
  prepare_sync ();
  let sync_table = create_sync_table () in
  let inotify = create_inotifylib_watcher ~sync_table ~event_queue should_exclude in
  Inotify.add inotify (Lazy.force Fs_sync.special_dir);
  { kind = Inotify inotify; sync_table }
;;

let fsevents_callback ?exclusion_paths event_queue ~f events =
  let skip_path =
    (* excluding a [path] will exclude children under [path] but not [path]
       itself. Hence we need to skip [path] manually *)
    match exclusion_paths with
    | None -> fun _ -> false
    | Some paths -> fun p -> List.mem paths p ~equal:Path.equal
  in
  let events =
    List.filter_map events ~f:(fun event ->
      let path =
        Fsevents.Event.path event |> Path.of_string |> Path.Expert.try_localize_external
      in
      if skip_path path then None else f event path)
  in
  send_events event_queue events
;;

let fsevents ?exclusion_paths ~latency ~paths event_queue f =
  let fsevents =
    let paths = List.map paths ~f:Path.to_absolute_filename in
    Fsevents.create ~latency ~paths ~f:(fsevents_callback ?exclusion_paths event_queue ~f)
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
    Some (Event.Fs_memo_event (Fs_memo_event.create ~kind ~path)))
;;

let create_fsevents ?(latency = Time.Span.of_secs 0.2) ~event_queue ~should_exclude () =
  prepare_sync ();
  let sync_table = create_sync_table () in
  let sync =
    (* Keep the original event path for consuming the sync file; use the
       localized path only to check whether the event is in the build directory. *)
    fsevents
      ~latency
      ~paths:[ Path.build (Lazy.force Fs_sync.special_dir_path) ]
      event_queue
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
    fsevents ~latency event_queue ~exclusion_paths ~paths on_event
  in
  let cv = Condition.create () in
  let dispatch_queue_ref = ref None in
  let mutex = Mutex.create () in
  let (_ : Thread.t) =
    Thread0.spawn ~name:"file-watcher" (fun () ->
      let dispatch_queue = Fsevents.Dispatch_queue.create () in
      Mutex.protect mutex (fun () ->
        dispatch_queue_ref := Some dispatch_queue;
        Condition.signal cv);
      Fsevents.start source dispatch_queue;
      Fsevents.start sync dispatch_queue;
      match Fsevents.Dispatch_queue.wait_until_stopped dispatch_queue with
      | Ok () -> ()
      | Error exn -> Code_error.raise "fsevents callback raised" [ "exn", Exn.to_dyn exn ])
  in
  let external_ = Watch_trie.empty in
  let dispatch_queue =
    Mutex.protect mutex (fun () ->
      while !dispatch_queue_ref = None do
        Condition.wait cv mutex
      done);
    Option.value_exn !dispatch_queue_ref
  in
  { kind =
      Fsevents { latency; event_queue; sync; source; external_; dispatch_queue; on_event }
  ; sync_table
  }
;;

let fswatch_win_callback ~event_queue ~sync_table ~should_exclude event =
  let filename =
    let dir = Fswatch_win.Event.directory event in
    Filename.concat dir (Fswatch_win.Event.path event)
  in
  let localized_path = Path.Expert.try_localize_external (Path.of_string filename) in
  match localized_path with
  | In_build_dir _ ->
    if Fs_sync.is_special_file_fsevents localized_path
    then (
      match Fswatch_win.Event.action event with
      | Added | Modified ->
        (match Fs_sync.consume_event sync_table filename with
         | None -> ()
         | Some id -> send_events event_queue [ Event.Sync id ])
      | Removed | Renamed_new | Renamed_old -> ())
  | path ->
    let normalized_filename =
      String.concat
        ~sep:"/"
        (String.split_on_char ~sep:'\\' (String.lowercase_ascii filename))
    in
    if not (should_exclude normalized_filename)
    then (
      let kind =
        match Fswatch_win.Event.action event with
        | Added | Renamed_new -> Fs_memo_event.Created
        | Removed | Renamed_old -> Deleted
        | Modified -> File_changed
      in
      send_events event_queue [ Event.Fs_memo_event (Fs_memo_event.create ~kind ~path) ])
;;

let create_fswatch_win ~event_queue ~debounce_interval:sleep ~should_exclude =
  prepare_sync ();
  let sync_table = create_sync_table () in
  let t = Fswatch_win.create () in
  Fswatch_win.add t (Path.to_absolute_filename Path.root);
  let (_ : Thread.t) =
    Thread0.spawn ~name:"file-watcher" (fun () ->
      while true do
        let events = Fswatch_win.wait t ~sleep in
        List.iter
          ~f:(fswatch_win_callback ~event_queue ~sync_table ~should_exclude)
          events
      done)
  in
  { kind = Fswatch_win { t }; sync_table }
;;

let create_default ?fsevents_debounce ~watch_exclusions ~event_queue () =
  let should_exclude = create_should_exclude_predicate ~watch_exclusions in
  match select_watcher_backend () with
  | `Fswatch _ as backend ->
    create_external_fswatch ~event_queue ~backend ~watch_exclusions
  | `Fsevents ->
    create_fsevents ?latency:fsevents_debounce ~event_queue ~should_exclude ()
  | `Inotify_lib -> create_inotifylib ~event_queue ~should_exclude
  | `Fswatch_win ->
    create_fswatch_win
      ~event_queue
      ~should_exclude
      ~debounce_interval:500 (* milliseconds *)
;;

(* Return the parent directory of [ext] if [ext] denotes a file. *)
let parent_directory ext =
  let rec loop p =
    if Fpath.is_directory (Path.External.to_string p)
    then Some p
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

let%expect_test "parent_directory" =
  let print_parent_directory path =
    let path = Path.External.of_string path in
    let result =
      match parent_directory path with
      | None -> "none"
      | Some dir -> Filename.basename (Path.External.to_string dir)
    in
    Printf.printf "%s -> %s\n" (Filename.basename (Path.External.to_string path)) result
  in
  let cwd = Unix.getcwd () in
  let dir = Filename.concat cwd "parent-directory" in
  (try Unix.mkdir dir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let file = Filename.concat dir "file.ml" in
  close_out (open_out file);
  print_parent_directory dir;
  print_parent_directory file;
  [%expect
    {|
    parent-directory -> parent-directory
    file.ml -> parent-directory
    |}]
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
            lazy
              (fsevents
                 ~latency:f.latency
                 f.event_queue
                 ~paths:[ Path.external_ ext ]
                 f.on_event)
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
    (try Ok (Inotify.add inotify (Path.to_string path)) with
     | Unix.Unix_error (ENOENT, _, _) -> Error `Does_not_exist)
  | Fswatch_win fswatch ->
    (match path with
     | In_build_dir _ -> Code_error.raise "attempted to watch a directory in build" []
     | Path.In_source_tree _ -> Ok ()
     | External ext ->
       (match parent_directory ext with
        | None -> Ok ()
        | Some ext ->
          Fswatch_win.add fswatch.t (Path.External.to_string ext);
          Ok ()))
;;

let emit_sync t = Fs_sync.emit t.sync_table

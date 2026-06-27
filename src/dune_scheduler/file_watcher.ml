open Import
module Scheduler_event = Event
module Fs_memo_event = Scheduler_event.Fs_memo_event
module Event = Scheduler_event.File_watcher_event

module Sync = struct
  module Id = Id.Make ()

  type t =
    { id : Id.t
    ; ivar : unit Fiber.Ivar.t
    }

  module Table = struct
    type nonrec t =
      { table : (string, t) Table.t
      ; mutex : Mutex.t
      }

    let create () = { table = Table.create (module String) 64; mutex = Mutex.create () }

    let create_sync_file t ~fn ~sync ~path =
      Mutex.protect t.mutex (fun () ->
        Table.set t.table fn sync;
        match
          Unix.close (Unix.openfile path [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ] 0o666)
        with
        | () -> ()
        | exception exn ->
          Table.remove t.table fn;
          Exn.reraise exn)
    ;;

    let consume_event t path =
      Mutex.protect t.mutex (fun () ->
        let basename = Filename.basename path in
        match Table.find t.table basename with
        | None -> None
        | Some sync ->
          Fpath.unlink_no_err path;
          Table.remove t.table basename;
          Some sync)
    ;;
  end

  let special_dir_path = lazy (Path.Build.relative Path.Build.root ".sync")
  let special_dir = lazy (Lazy.force special_dir_path |> Path.Build.to_string)

  let flush sync_table =
    let ivar = Fiber.Ivar.create () in
    let id = Id.gen () in
    let sync = { id; ivar } in
    let fn = Id.to_int id |> string_of_int in
    let path = Filename.concat (Lazy.force special_dir) fn in
    Table.create_sync_file sync_table ~fn ~sync ~path;
    Fiber.Ivar.read ivar
  ;;

  let is_special_file ~path_as_reported_by_file_watcher =
    (* We use string matching here and that's fine because we match on the path
       reported by the file watcher backend which will always report the same
       string that was used to setup the watch. *)
    Filename.dirname path_as_reported_by_file_watcher = Lazy.force special_dir
  ;;

  (* fsevents always reports absolute paths. therefore, we need callers to make
     an effort to determine if an absolute path is in fact in the build dir *)
  let is_special_file_fsevents (path : Path.t) =
    match path with
    | In_source_tree _ | External _ -> false
    | In_build_dir build_path ->
      (match Path.Build.parent build_path with
       | None -> false
       | Some dir -> Path.Build.equal dir (Lazy.force special_dir_path))
  ;;
end

type backend_event =
  | Filesystem_event of Event.t
  | Sync of Sync.t
  | Watcher_terminated

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
      | "" :: comps -> List.filter comps ~f:(fun s -> not (String.is_empty s))
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

  let%test_module "tests" =
    (module struct
      let of_string = Path.External.of_string

      let print_entries ?label entries =
        Option.iter label ~f:(fun label -> Printf.printf "%s:\n" label);
        List.map entries ~f:(fun (path, value) -> Path.External.to_string path, value)
        |> List.sort ~compare:(fun (x, _) (y, _) -> String.compare x y)
        |> List.iter ~f:(fun (path, value) -> Printf.printf "%s -> %s\n" path value)
      ;;

      let insert_lazy_exn t path value =
        match add t (of_string path) value with
        | Under_existing_node -> Code_error.raise "unexpected existing node" []
        | Inserted { new_t; _ } -> new_t
      ;;

      let insert_exn t path value = insert_lazy_exn t path (lazy value)

      let sample () =
        let t = insert_exn empty "/a/b" "ab" in
        let t = insert_exn t "/a/c" "ac" in
        insert_exn t "/d" "d"
      ;;

      let expect_under_existing t path value message =
        match add t (of_string path) value with
        | Under_existing_node -> Printf.printf "%s\n" message
        | Inserted _ -> Code_error.raise "unexpected insertion" []
      ;;

      let%expect_test "inserting independent watches" =
        print_entries (to_list (sample ()));
        [%expect
          {|
          /a/b -> ab
          /a/c -> ac
          /d -> d
          |}]
      ;;

      let%expect_test "inserting an ancestor removes descendants" =
        match add (sample ()) (of_string "/a") (lazy "a") with
        | Under_existing_node -> Code_error.raise "unexpected existing node" []
        | Inserted { new_t; removed } ->
          print_entries ~label:"removed" removed;
          print_entries ~label:"remaining" (to_list new_t);
          [%expect
            {|
            removed:
            /a/b -> ab
            /a/c -> ac
            remaining:
            /a -> a
            /d -> d
            |}]
      ;;

      let%expect_test "existing watches cover descendants and themselves" =
        let forced = ref [] in
        let value s =
          lazy
            (forced := s :: !forced;
             s)
        in
        let t = insert_lazy_exn empty "/a" (value "a") in
        let print_forced () =
          !forced |> List.rev |> String.concat ~sep:", " |> Printf.printf "forced: %s\n"
        in
        print_forced ();
        expect_under_existing t "/a/b" (value "ab") "descendant ignored";
        expect_under_existing t "/a" (value "a2") "same path ignored";
        print_forced ();
        [%expect
          {|
          forced: a
          descendant ignored
          same path ignored
          forced: a
          |}]
      ;;
    end)
  ;;

  let%expect_test "root watch covers descendants" =
    let p = Path.External.of_string in
    let t =
      match add empty (p "/") (lazy "root") with
      | Under_existing_node -> Code_error.raise "unexpected existing node" []
      | Inserted { new_t; removed = [] } -> new_t
      | Inserted { removed = _ :: _; _ } ->
        Code_error.raise "unexpected removed watches" []
    in
    (match add t (p "/tmp") (lazy "tmp") with
     | Under_existing_node -> Printf.printf "covered by root\n"
     | Inserted _ -> Printf.printf "inserted separate watch\n");
    [%expect {| covered by root |}]
  ;;
end

type kind =
  | Fswatch of { pid : Pid.t }
  | Fsevents of
      { mutable external_ : Fsevents.t Watch_trie.t
      ; dispatch_queue : Fsevents.Dispatch_queue.t
      ; source : Fsevents.t
      ; sync : Fsevents.t
      ; latency : Time.Span.t
      ; on_event : Fsevents.Event.t -> Path.t -> backend_event option
      }
  | Inotify of Inotify.t
  | Fswatch_win of { t : Fswatch_win.t }

type t =
  { kind : kind
  ; sync_table : Sync.Table.t
    (* Pending fs sync operations indexed by the special sync filename. *)
  ; events : Event.t list Thread_safe_channel.t
  }

let create_should_exclude_predicate ~watch_exclusions =
  (* TODO we should really take the predicate directly and not depend on
     regular expressions in our file watching component *)
  Re.execp (Re.compile (Re.alt (List.map watch_exclusions ~f:Re.Posix.re)))
;;

let standard_watch_exclusions =
  [ {|^_opam|}
  ; {|/_opam|}
  ; {|^_esy|}
  ; {|/_esy|}
  ; {|^\.#.*|} (* Such files can be created by Emacs and also Dune itself. *)
  ; {|/\.#.*|}
  ; {|~$|}
  ; {|^#[^#]*#$|}
  ; {|/#[^#]*#$|}
  ; {|^4913$|} (* https://github.com/neovim/neovim/issues/3460 *)
  ; {|/4913$|}
  ; {|/.git|}
  ; {|/.hg|}
  ; {|:/windows|}
  ]
;;

let%expect_test "watch exclusion patterns" =
  let should_exclude =
    create_should_exclude_predicate ~watch_exclusions:standard_watch_exclusions
  in
  let test string =
    Printf.printf "should_exclude(%s) = %b\n" string (should_exclude string)
  in
  test "file.ml";
  test "dir/file.ml";
  test "4913";
  test "dir/4913";
  test "4913.ml";
  test "84913";
  test "_opam";
  test "dir/_opam";
  test "this_is_not_opam";
  test "#file#";
  test "dir/#file#";
  test "dir/#subdir#/file";
  test ".#file";
  test ".#foobar.ml";
  test "dir/.#file";
  test "dir/.#subdir/file";
  [%expect
    {|
    should_exclude(file.ml) = false
    should_exclude(dir/file.ml) = false
    should_exclude(4913) = true
    should_exclude(dir/4913) = true
    should_exclude(4913.ml) = false
    should_exclude(84913) = false
    should_exclude(_opam) = true
    should_exclude(dir/_opam) = true
    should_exclude(this_is_not_opam) = false
    should_exclude(#file#) = true
    should_exclude(dir/#file#) = true
    should_exclude(dir/#subdir#/file) = false
    should_exclude(.#file) = true
    should_exclude(.#foobar.ml) = true
    should_exclude(dir/.#file) = true
    should_exclude(dir/.#subdir/file) = true
    |}]
;;

let process_inotify_event (event : Inotify.Event.t) should_exclude : backend_event list =
  let create_event_unless_excluded ~kind ~path =
    if should_exclude path
    then []
    else (
      let path = Path.of_string path in
      [ Filesystem_event (Event.Fs_memo_event (Fs_memo_event.create ~kind ~path)) ])
  in
  match event with
  | Queue_overflow -> [ Filesystem_event Queue_overflow ]
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

let event_to_trace_event = function
  | Sync { id; _ } -> `Sync (Sync.Id.to_int id)
  | Watcher_terminated -> `Watcher_terminated
  | Filesystem_event Queue_overflow -> `Queue_overflow
  | Filesystem_event (Fs_memo_event { path; kind }) -> `File (path, kind)
;;

let trace_event_to_dyn = function
  | `Sync id -> Dyn.variant "Sync" [ Dyn.int id ]
  | `Queue_overflow -> Dyn.variant "Queue_overflow" []
  | `Watcher_terminated -> Dyn.variant "Watcher_terminated" []
  | `File (path, kind) ->
    Dyn.variant
      "File"
      [ Path.to_dyn path; Repr.to_dyn Dune_trace.File_watcher_event.kind_repr kind ]
;;

let emit_events events =
  Dune_trace.emit_all ~buffered:true File_watcher (fun () ->
    List.map events ~f:(fun event ->
      Dune_trace.File_watcher_event.to_event (event_to_trace_event event)))
;;

let log_dropped_events events =
  Log.info
    "file watcher events dropped: channel closed"
    [ "events", Dyn.list trace_event_to_dyn (List.map events ~f:event_to_trace_event) ]
;;

let dispatch_backend_events events backend_events =
  let send_file_events file_events =
    match List.rev file_events with
    | [] -> ()
    | file_events ->
      let backend_events =
        List.map file_events ~f:(fun event -> Filesystem_event event)
      in
      (match Thread_safe_channel.write events file_events with
       | `Ok -> emit_events backend_events
       | `Closed -> log_dropped_events backend_events)
  in
  let rec loop file_events = function
    | [] -> send_file_events file_events
    | Filesystem_event event :: backend_events ->
      loop (event :: file_events) backend_events
    | Sync ({ ivar; _ } as sync) :: backend_events ->
      send_file_events file_events;
      let backend_event = Sync sync in
      (match Thread_safe_channel.write_fill events (Fiber.Fill (ivar, ())) with
       | `Ok -> emit_events [ backend_event ]
       | `Closed -> log_dropped_events [ backend_event ]);
      loop [] backend_events
    | Watcher_terminated :: _ ->
      send_file_events file_events;
      Thread_safe_channel.close events;
      emit_events [ Watcher_terminated ]
  in
  loop [] backend_events
;;

let close t = Thread_safe_channel.close t.events
let read t = Thread_safe_channel.read t.events

let child_pids t =
  match t.kind with
  | Fswatch { pid; _ } -> Some pid
  | Inotify _ | Fsevents _ | Fswatch_win _ -> None
;;

let shutdown t =
  close t;
  match t.kind with
  | Inotify _ -> ()
  | Fswatch { pid; _ } -> Pid.kill pid `Pid Term
  | Fswatch_win { t } -> Fswatch_win.shutdown t
  | Fsevents fsevents ->
    Fsevents.stop fsevents.source;
    Fsevents.stop fsevents.sync;
    Watch_trie.to_list fsevents.external_
    |> List.iter ~f:(fun (_, fs) -> Fsevents.stop fs)
;;

let command ~backend ~watch_exclusions =
  let inotify_special_path = Lazy.force Sync.special_dir in
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
      | FreeBSD | DragonFly -> [ User_message.command "pkg install fswatch-mon" ]
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
    | Linux | Darwin | FreeBSD | OpenBSD | NetBSD | DragonFly | Haiku | Other ->
      fswatch_backend ())
;;

let prepare_sync () =
  let dir = Lazy.force Sync.special_dir in
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
    Spawn.spawn () ~prog ~argv ~stdout:w_stdout |> Pid.of_int_exn
  in
  Unix.close w_stdout;
  Unix.in_channel_of_descr r_stdout, pid
;;

let create_inotifylib_watcher ~sync_table ~event_channel should_exclude =
  let { Sync.Table.mutex; _ } = sync_table in
  Inotify.create
    ~mutex
    ~modify_event_selector:`Closed_writable_fd
    ~emit_events:(fun events ->
      let events =
        List.concat_map events ~f:(fun event ->
          let is_fs_sync_event_generated_by_dune =
            match (event : Inotify.Event.t) with
            | Modified path | Created path | Unlinked path ->
              Option.some_if
                (Sync.is_special_file ~path_as_reported_by_file_watcher:path)
                path
            | Moved _ | Queue_overflow -> None
          in
          match is_fs_sync_event_generated_by_dune with
          | None -> process_inotify_event event should_exclude
          | Some path ->
            (match Sync.Table.consume_event sync_table path with
             | None -> []
             | Some sync -> [ Sync sync ]))
      in
      dispatch_backend_events event_channel events)
;;

let create_external_fswatch ~sync_table ~event_channel ~backend ~watch_exclusions =
  let debounce_interval = Time.Span.of_secs 0.5 in
  let pending_events = ref [] in
  let event_mtx = Mutex.create () in
  let event_cv = Condition.create () in
  let pipe, pid = spawn_external_watcher ~backend ~watch_exclusions in
  let (_ : Thread.t) =
    Thread0.spawn ~name:"file-watcher" (fun () ->
      let enqueue events =
        Mutex.protect event_mtx (fun () ->
          pending_events := events :: !pending_events;
          Condition.signal event_cv)
      in
      let rec loop () =
        match input_line pipe with
        | exception End_of_file -> enqueue [ Watcher_terminated ]
        | path_s ->
          let events =
            if Sync.is_special_file ~path_as_reported_by_file_watcher:path_s
            then (
              match Sync.Table.consume_event sync_table path_s with
              | None -> []
              | Some sync -> [ Sync sync ])
            else (
              let path = Path.Expert.try_localize_external (Path.of_string path_s) in
              [ Filesystem_event
                  (Fs_memo_event (Fs_memo_event.create ~kind:File_changed ~path))
              ])
          in
          enqueue events;
          loop ()
      in
      loop ())
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
      let events =
        Mutex.protect event_mtx (fun () ->
          while List.is_empty !pending_events do
            Condition.wait event_cv event_mtx
          done;
          let events = List.rev !pending_events |> List.concat in
          pending_events := [];
          events)
      in
      dispatch_backend_events event_channel events;
      Thread.delay (Time.Span.to_secs debounce_interval);
      buffer_thread ()
    in
    Thread0.spawn ~name:"file-watcher-buffer" buffer_thread
  in
  Fswatch { pid }
;;

let create_inotifylib ~sync_table ~event_channel ~should_exclude =
  prepare_sync ();
  let inotify = create_inotifylib_watcher ~sync_table ~event_channel should_exclude in
  Inotify.add inotify (Lazy.force Sync.special_dir);
  Inotify inotify
;;

let fsevents_callback ?exclusion_paths event_channel ~f events =
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
  dispatch_backend_events event_channel events
;;

let fsevents ?exclusion_paths ~latency ~paths event_channel f =
  let fsevents =
    let paths = List.map paths ~f:Path.to_absolute_filename in
    Fsevents.create
      ~latency
      ~paths
      ~f:(fsevents_callback ?exclusion_paths event_channel ~f)
  in
  Option.iter exclusion_paths ~f:(fun paths ->
    let paths = List.rev_map paths ~f:Path.to_absolute_filename in
    Fsevents.set_exclusion_paths fsevents ~paths);
  fsevents
;;

let fsevents_standard_event_impl
      ~should_exclude
      ~(action : Fsevents.Event.Action.t)
      ~(kind : Fsevents.Event.Kind.t)
      path
  =
  if Option.is_some (Path.as_in_build_dir path) || should_exclude (Path.to_string path)
  then None
  else (
    let fs_memo_event kind =
      Some (Filesystem_event (Event.Fs_memo_event (Fs_memo_event.create ~kind ~path)))
    in
    match kind with
    | Dir_and_descendants -> Some (Filesystem_event Event.Queue_overflow)
    | Dir ->
      (match action with
       | Remove | Rename | Unknown ->
         (* FSEvents can report directory-wide or ambiguous changes without
            listing all affected descendants. Until Fs_memo supports subtree
            invalidation, over-invalidate by clearing the filesystem caches. *)
         Some (Filesystem_event Queue_overflow)
       | Create -> fs_memo_event Created
       | Modify -> fs_memo_event Unknown)
    | File ->
      (match action with
       | Rename | Unknown -> fs_memo_event Unknown
       | Create -> fs_memo_event Created
       | Remove -> fs_memo_event Deleted
       | Modify -> fs_memo_event File_changed))
;;

let%expect_test "fsevents standard event handling" =
  let print
        ?(should_exclude = Fun.const false)
        ?(path = Path.source (Path.Source.relative Path.Source.root "x"))
        (action : Fsevents.Event.Action.t)
        (kind : Fsevents.Event.Kind.t)
    =
    match fsevents_standard_event_impl ~should_exclude ~action ~kind path with
    | None -> print_endline "None"
    | Some (Filesystem_event Event.Queue_overflow) -> print_endline "Queue_overflow"
    | Some (Filesystem_event (Event.Fs_memo_event event)) ->
      Fs_memo_event.to_dyn event |> Dyn.to_string |> print_endline
    | Some (Sync _ | Watcher_terminated) ->
      Code_error.raise "unexpected fsevents standard event" []
  in
  print Modify File;
  print Rename Dir;
  print Remove Dir;
  print Unknown Dir;
  print Modify Dir_and_descendants;
  print Create File ~path:(Path.build (Path.Build.relative Path.Build.root "x"));
  print Create File ~should_exclude:(Fun.const true);
  [%expect
    {|
    { path = In_source_tree "x"; kind = File_changed }
    Queue_overflow
    Queue_overflow
    Queue_overflow
    Queue_overflow
    None
    None
    |}]
;;

let create_fsevents
      ~sync_table
      ~event_channel
      ?(latency = Time.Span.of_secs 0.2)
      ~should_exclude
      ()
  =
  prepare_sync ();
  let sync =
    (* Keep the original event path for consuming the sync file; use the
       localized path only to check whether the event is in the build directory. *)
    fsevents
      ~latency
      ~paths:[ Path.build (Lazy.force Sync.special_dir_path) ]
      event_channel
      (fun event localized_path ->
         let path = Fsevents.Event.path event in
         match Sync.is_special_file_fsevents localized_path with
         | false -> None
         | true ->
           (match Fsevents.Event.action event with
            | Remove -> None
            | Rename | Unknown | Create | Modify ->
              Sync.Table.consume_event sync_table path
              |> Option.map ~f:(fun sync -> Sync sync)))
  in
  let on_event event path =
    fsevents_standard_event_impl
      ~should_exclude
      ~action:(Fsevents.Event.action event)
      ~kind:(Fsevents.Event.kind event)
      path
  in
  let source =
    let paths = [ Path.root ] in
    let exclusion_paths =
      Path.(build Build.root)
      :: ([ "_esy"; "_opam"; ".git"; ".hg" ]
          |> List.rev_map ~f:(Path.relative (Path.source Path.Source.root)))
    in
    fsevents ~latency event_channel ~exclusion_paths ~paths on_event
  in
  let cv = Condition.create () in
  let dispatch_queue_ref = ref None in
  let mutex = Mutex.create () in
  let (_ : Thread.t) =
    let signal_dispatch_queue result =
      Mutex.protect mutex (fun () ->
        dispatch_queue_ref := Some result;
        Condition.signal cv)
    in
    let start_streams dispatch_queue =
      Fsevents.start source dispatch_queue;
      try Fsevents.start sync dispatch_queue with
      | exn ->
        Fsevents.stop source;
        Exn.reraise exn
    in
    Thread0.spawn ~name:"file-watcher" (fun () ->
      match
        let dispatch_queue = Fsevents.Dispatch_queue.create () in
        start_streams dispatch_queue;
        dispatch_queue
      with
      | exception exn -> signal_dispatch_queue (Error (Exn_with_backtrace.capture exn))
      | dispatch_queue ->
        signal_dispatch_queue (Ok dispatch_queue);
        (match Fsevents.Dispatch_queue.wait_until_stopped dispatch_queue with
         | Ok () -> ()
         | Error exn ->
           Log.warn "FSEvents watcher error" [ "exn", Exn.to_dyn exn ];
           dispatch_backend_events event_channel [ Watcher_terminated ]))
  in
  let dispatch_queue =
    match
      Mutex.protect mutex (fun () ->
        while Option.is_none !dispatch_queue_ref do
          Condition.wait cv mutex
        done;
        Option.value_exn !dispatch_queue_ref)
    with
    | Ok dispatch_queue -> dispatch_queue
    | Error exn -> Exn_with_backtrace.reraise exn
  in
  Fsevents
    { latency; sync; source; external_ = Watch_trie.empty; dispatch_queue; on_event }
;;

let fswatch_win_event ~sync_table ~should_exclude event =
  let filename =
    let dir = Fswatch_win.Event.directory event in
    Filename.concat dir (Fswatch_win.Event.path event)
  in
  let localized_path = Path.Expert.try_localize_external (Path.of_string filename) in
  match localized_path with
  | In_build_dir _ ->
    if Sync.is_special_file_fsevents localized_path
    then (
      match Fswatch_win.Event.action event with
      | Added | Modified ->
        Sync.Table.consume_event sync_table filename
        |> Option.map ~f:(fun sync -> Sync sync)
      | Removed | Renamed_new | Renamed_old -> None)
    else None
  | path ->
    let normalized_filename =
      String.concat
        ~sep:"/"
        (String.split_on_char ~sep:'\\' (String.lowercase_ascii filename))
    in
    if should_exclude normalized_filename
    then None
    else
      Some
        (let kind =
           match Fswatch_win.Event.action event with
           | Added | Renamed_new -> Dune_trace.File_watcher_event.Created
           | Removed | Renamed_old -> Deleted
           | Modified -> File_changed
         in
         Filesystem_event (Fs_memo_event (Fs_memo_event.create ~kind ~path)))
;;

let create_fswatch_win ~sync_table ~event_channel ~debounce_interval:sleep ~should_exclude
  =
  prepare_sync ();
  let t = Fswatch_win.create () in
  Fswatch_win.add t (Path.to_absolute_filename Path.root);
  let (_ : Thread.t) =
    Thread0.spawn ~name:"file-watcher" (fun () ->
      while true do
        let events = Fswatch_win.wait t ~sleep in
        let events =
          List.filter_map events ~f:(fswatch_win_event ~sync_table ~should_exclude)
        in
        dispatch_backend_events event_channel events
      done)
  in
  Fswatch_win { t }
;;

let create ?fsevents_debounce ~watch_exclusions ~event_queue () =
  let events = Thread_safe_channel.create event_queue in
  let sync_table = Sync.Table.create () in
  let should_exclude = create_should_exclude_predicate ~watch_exclusions in
  { kind =
      (match select_watcher_backend () with
       | `Fswatch _ as backend ->
         create_external_fswatch
           ~sync_table
           ~event_channel:events
           ~backend
           ~watch_exclusions
       | `Fsevents ->
         create_fsevents
           ~sync_table
           ~event_channel:events
           ?latency:fsevents_debounce
           ~should_exclude
           ()
       | `Inotify_lib ->
         create_inotifylib ~sync_table ~event_channel:events ~should_exclude
       | `Fswatch_win ->
         create_fswatch_win
           ~sync_table
           ~event_channel:events
           ~should_exclude
           ~debounce_interval:500)
  ; sync_table
  ; events
  }
;;

let flush t = Sync.flush t.sync_table

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
                 t.events
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

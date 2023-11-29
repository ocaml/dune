open Stdune
module Event = Fsevents.Event

module Logger : sig
  type t

  val create : unit -> t
  val printfn : t -> ('a, unit, string, unit) format4 -> 'a
  val flush : t -> unit
end = struct
  type t = { messages : string Queue.t }

  let create () = { messages = Queue.create () }
  let printfn t fmt = Printf.ksprintf (fun s -> Queue.push t.messages s) fmt

  let flush t =
    let rec loop () =
      match Queue.pop t.messages with
      | None -> ()
      | Some s ->
        print_endline s;
        loop ()
    in
    loop ()
  ;;
end

let timeout_thread ~wait f =
  let spawn () =
    Thread.delay wait;
    f ()
  in
  let (_ : Thread.t) = Thread.create spawn () in
  ()
;;

let start_filename = ".dune_fsevents_start"
let end_filename = ".dune_fsevents_end"

let emit_start dir =
  ignore (Fpath.mkdir_p dir);
  Io.String_path.write_file (Filename.concat dir start_filename) ""
;;

let emit_stop dir =
  ignore (Fpath.mkdir_p dir);
  Io.String_path.write_file (Filename.concat dir end_filename) ""
;;

let test f =
  let cv = Condition.create () in
  let mutex = Mutex.create () in
  let finished = ref false in
  let finish () =
    Mutex.lock mutex;
    finished := true;
    Condition.signal cv;
    Mutex.unlock mutex
  in
  timeout_thread ~wait:3.0 (fun () ->
    Mutex.lock mutex;
    if not !finished
    then (
      Format.eprintf "Test timed out@.";
      finished := true;
      Condition.signal cv);
    Mutex.unlock mutex);
  let test () =
    let dir = Temp.create Dir ~prefix:"fsevents_dune" ~suffix:"" in
    let old = Sys.getcwd () in
    Sys.chdir (Path.to_string dir);
    Exn.protect
      ~f:(fun () -> f finish)
      ~finally:(fun () ->
        Sys.chdir old;
        Temp.destroy Dir dir)
  in
  let (_ : Thread.t) = Thread.create test () in
  Mutex.lock mutex;
  while not !finished do
    Condition.wait cv mutex
  done;
  Mutex.unlock mutex
;;

let print_event ~logger ~cwd e =
  let dyn =
    let open Dyn in
    record
      [ "action", Event.dyn_of_action (Event.action e)
      ; "kind", Event.dyn_of_kind (Event.kind e)
      ; ( "path"
        , string
            (let path = Event.path e in
             match String.drop_prefix ~prefix:cwd path with
             | None -> path
             | Some p -> "$TESTCASE_ROOT" ^ p) )
      ]
  in
  Logger.printfn logger "> %s" (Dyn.to_string dyn)
;;

let make_callback sync ~f =
  (* hack to skip the first event if it's creating the temp dir *)
  let state = ref `Looking_start in
  fun events ->
    let is_marker event filename =
      Event.kind event = File
      && Filename.basename (Event.path event) = filename
      && Event.action event = Create
    in
    let events =
      List.fold_left events ~init:[] ~f:(fun acc event ->
        match !state with
        | `Looking_start ->
          if is_marker event start_filename
          then (
            state := `Keep;
            sync#start);
          acc
        | `Finish -> acc
        | `Keep ->
          if is_marker event end_filename
          then (
            state := `Finish;
            sync#stop;
            acc)
          else event :: acc)
    in
    match events with
    | [] -> ()
    | _ -> List.rev events |> List.iter ~f:(f ~logger:sync#logger)
;;

type test_config =
  { on_event : logger:Logger.t -> Event.t -> unit
  ; exclusion_paths : string list
  ; dir : string
  }

let default_test_config cwd =
  { on_event = print_event ~cwd; dir = cwd; exclusion_paths = [] }
;;

let test_with_multiple_fsevents ~setup ~test:f =
  test (fun finish ->
    let cwd = Sys.getcwd () in
    let make_sync t config =
      let logger = Logger.create () in
      object
        val mutable started = false
        val mutable stopped = false
        method logger = logger
        method started = started
        method stopped = stopped
        method start = started <- true

        method stop =
          stopped <- true;
          Fsevents.stop (Option.value_exn !t)

        method emit_start = if not started then emit_start config.dir
        method emit_stop = if not stopped then emit_stop config.dir
      end
    in
    let configs = setup ~cwd (default_test_config cwd) in
    let fsevents, syncs =
      List.map configs ~f:(fun config ->
        let t = ref None in
        let sync = make_sync t config in
        let res =
          Fsevents.create
            ~paths:[ config.dir ]
            ~latency:0.0
            ~f:(make_callback sync ~f:config.on_event)
        in
        (match config.exclusion_paths with
         | [] -> ()
         | paths ->
           (* apple doesn't like [paths] empty *)
           Fsevents.set_exclusion_paths res ~paths);
        t := Some res;
        res, sync)
      |> List.unzip
    in
    let dispatch_queue = Fsevents.Dispatch_queue.create () in
    List.iter fsevents ~f:(fun f -> Fsevents.start f dispatch_queue);
    let (t : Thread.t) =
      Thread.create
        (fun () ->
          let rec await ~emit ~continue = function
            | [] -> ()
            | xs ->
              List.iter xs ~f:emit;
              Unix.sleepf 0.2;
              await ~emit ~continue (List.filter xs ~f:continue)
          in
          await
            ~emit:(fun sync -> sync#emit_start)
            ~continue:(fun sync -> not sync#started)
            syncs;
          f ();
          await
            ~emit:(fun sync -> sync#emit_stop)
            ~continue:(fun sync -> not sync#stopped)
            syncs)
        ()
    in
    (match Fsevents.Dispatch_queue.wait_until_stopped dispatch_queue with
     | Error Exit -> print_endline "[EXIT]"
     | Error _ -> assert false
     | Ok () -> ());
    Thread.join t;
    List.iter syncs ~f:(fun c -> Logger.flush c#logger);
    finish ())
;;

let test_with_operations ?on_event ?exclusion_paths f =
  test_with_multiple_fsevents ~test:f ~setup:(fun ~cwd config ->
    let config =
      match exclusion_paths with
      | None -> config
      | Some f -> { config with exclusion_paths = f cwd }
    in
    [ (match on_event with
       | None -> config
       | Some on_event -> { config with on_event })
    ])
;;

let%expect_test "file create event" =
  test_with_operations (fun () -> Io.String_path.write_file "./file" "foobar");
  [%expect {|
    > { action = "Create"; kind = "File"; path = "$TESTCASE_ROOT/file" } |}]
;;

let%expect_test "dir create event" =
  test_with_operations (fun () -> ignore (Fpath.mkdir "./blahblah"));
  [%expect
    {|
    > { action = "Create"; kind = "Dir"; path = "$TESTCASE_ROOT/blahblah" } |}]
;;

let%expect_test "move file" =
  test_with_operations (fun () ->
    Io.String_path.write_file "old" "foobar";
    Unix.rename "old" "new");
  [%expect
    {|
    > { action = "Create"; kind = "File"; path = "$TESTCASE_ROOT/old" }
    > { action = "Rename"; kind = "File"; path = "$TESTCASE_ROOT/new" } |}]
;;

let%expect_test "raise inside callback" =
  test_with_operations
    ~on_event:(fun ~logger _ ->
      Logger.printfn logger "exiting.";
      raise Exit)
    (fun () ->
      Io.String_path.write_file "old" "foobar";
      Io.String_path.write_file "old" "foobar";
      (* Delay to allow the event handler callback to catch the exception
         before stopping the watcher. *)
      Unix.sleepf 1.0);
  [%expect {|
    [EXIT]
    exiting. |}]
;;

let%expect_test "set exclusion paths" =
  let run paths =
    let ignored = "ignored" in
    test_with_operations
      ~exclusion_paths:(fun cwd -> [ paths cwd ignored ])
      (fun () ->
        let (_ : Fpath.mkdir_p_result) = Fpath.mkdir_p ignored in
        Io.String_path.write_file (Filename.concat ignored "old") "foobar")
  in
  (* absolute paths work *)
  run Filename.concat;
  [%expect
    {|
    > { action = "Create"; kind = "Dir"; path = "$TESTCASE_ROOT/ignored" } |}];
  (* but relative paths do not *)
  run (fun _ name -> name);
  [%expect
    {|
    > { action = "Create"; kind = "Dir"; path = "$TESTCASE_ROOT/ignored" }
    > { action = "Create"; kind = "File"; path = "$TESTCASE_ROOT/ignored/old" } |}]
;;

let%expect_test "multiple fsevents" =
  test_with_multiple_fsevents
    ~setup:(fun ~cwd config ->
      let create path =
        let dir = Filename.concat cwd path in
        ignore (Fpath.mkdir dir);
        { config with dir }
      in
      [ create "foo"; create "bar" ])
    ~test:(fun () ->
      Io.String_path.write_file "foo/file" "";
      Io.String_path.write_file "bar/file" "";
      Io.String_path.write_file "xxx" "" (* this one is ignored *));
  [%expect
    {|
    > { action = "Create"; kind = "File"; path = "$TESTCASE_ROOT/foo/file" }
    > { action = "Create"; kind = "File"; path = "$TESTCASE_ROOT/bar/file" } |}]
;;

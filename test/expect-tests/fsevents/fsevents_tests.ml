open Stdune
module Event = Fsevents.Event

let timeout_thread ~wait f =
  let spawn () =
    Thread.delay wait;
    f ()
  in
  let (_ : Thread.t) = Thread.create spawn () in
  ()

let start_filename = ".dune_fsevents_start"

let end_filename = ".dune_fsevents_end"

let emit_start dir =
  ignore (Fpath.mkdir_p dir);
  Io.String_path.write_file (Filename.concat dir start_filename) ""

let emit_end dir =
  ignore (Fpath.mkdir_p dir);
  Io.String_path.write_file (Filename.concat dir end_filename) ""

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
      if not !finished then (
        Format.eprintf "Test timed out@.";
        finished := true;
        Condition.signal cv
      );
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

let print_event ~cwd e =
  let dyn =
    let open Dyn.Encoder in
    record
      [ ("action", Event.dyn_of_action (Event.action e))
      ; ("kind", Event.dyn_of_kind (Event.kind e))
      ; ( "path"
        , string
            (let path = Event.path e in
             match String.drop_prefix ~prefix:cwd path with
             | None -> path
             | Some p -> "$TESTCASE_ROOT" ^ p) )
      ]
  in
  printfn "> %s" (Dyn.to_string dyn)

let make_callback t ~f =
  (* hack to skip the first event if it's creating the temp dir *)
  let state = ref `Looking_start in
  fun events ->
    let t = Option.value_exn !t in
    let is_marker event filename =
      Event.kind event = File
      && Filename.basename (Event.path event) = filename
      && Event.action event = Create
    in
    let stop =
      lazy
        (Fsevents.stop t;
         Fsevents.RunLoop.stop (Option.value_exn (Fsevents.runloop t)))
    in
    let events =
      List.fold_left events ~init:[] ~f:(fun acc event ->
          match !state with
          | `Looking_start ->
            if is_marker event start_filename then state := `Keep;
            acc
          | `Finish -> acc
          | `Keep ->
            if is_marker event end_filename then (
              state := `Finish;
              Lazy.force stop;
              acc
            ) else
              event :: acc)
    in
    match events with
    | [] -> ()
    | _ -> f events

type test_config =
  { on_events : Event.t list -> unit
  ; exclusion_paths : string list
  ; dir : string
  }

let default_test_config cwd =
  { on_events = List.iter ~f:(print_event ~cwd)
  ; dir = cwd
  ; exclusion_paths = []
  }

let test_with_multiple_fsevents ~setup ~test:f =
  test (fun finish ->
      let cwd = Sys.getcwd () in
      let configs = setup ~cwd (default_test_config cwd) in
      let fsevents =
        List.map configs ~f:(fun config ->
            let t = ref None in
            let res =
              Fsevents.create ~paths:[ config.dir ] ~latency:0.0
                ~f:(make_callback t ~f:config.on_events)
            in
            t := Some res;
            res)
      in
      let runloop = Fsevents.RunLoop.in_current_thread () in
      List.iter fsevents ~f:(fun f -> Fsevents.start f runloop);
      let (_ : Thread.t) =
        Thread.create
          (fun () ->
            List.iter configs ~f:(fun config -> emit_start config.dir);
            f ();
            List.iter configs ~f:(fun config -> emit_end config.dir))
          ()
      in
      (match Fsevents.RunLoop.run_current_thread runloop with
      | Error Exit -> print_endline "[EXIT]"
      | Error _ -> assert false
      | Ok () -> ());
      List.iter fsevents ~f:Fsevents.stop;
      finish ())

let test_with_operations ?on_event ?exclusion_paths f =
  test_with_multiple_fsevents ~test:f ~setup:(fun ~cwd config ->
      let config =
        match exclusion_paths with
        | None -> config
        | Some f -> { config with exclusion_paths = f cwd }
      in
      [ (match on_event with
        | None -> config
        | Some on_event -> { config with on_events = List.iter ~f:on_event })
      ])

let%expect_test "file create event" =
  test_with_operations (fun () -> Io.String_path.write_file "./file" "foobar");
  [%expect
    {|
      > { action = "Unknown"; kind = "File"; path = "$TESTCASE_ROOT/file" } |}]

let%expect_test "dir create event" =
  test_with_operations (fun () -> ignore (Fpath.mkdir "./blahblah"));
  [%expect
    {|
      > { action = "Create"; kind = "Dir"; path = "$TESTCASE_ROOT/blahblah" } |}]

let%expect_test "move file" =
  test_with_operations (fun () ->
      Io.String_path.write_file "old" "foobar";
      Unix.rename "old" "new");
  [%expect
    {|
    > { action = "Unknown"; kind = "File"; path = "$TESTCASE_ROOT/new" }
    > { action = "Unknown"; kind = "File"; path = "$TESTCASE_ROOT/old" } |}]

let%expect_test "raise inside callback" =
  test_with_operations
    ~on_event:(fun _ ->
      print_endline "exiting.";
      raise Exit)
    (fun () ->
      Io.String_path.write_file "old" "foobar";
      Io.String_path.write_file "old" "foobar");
  [%expect {|
    exiting.
    [EXIT] |}]

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
      > { action = "Unknown"; kind = "File"; path = "$TESTCASE_ROOT/ignored/old" }
      > { action = "Create"; kind = "Dir"; path = "$TESTCASE_ROOT/ignored" } |}];
  (* but relative paths do not *)
  run (fun _ name -> name);
  [%expect
    {|
    > { action = "Unknown"; kind = "File"; path = "$TESTCASE_ROOT/ignored/old" }
    > { action = "Create"; kind = "Dir"; path = "$TESTCASE_ROOT/ignored" } |}]

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

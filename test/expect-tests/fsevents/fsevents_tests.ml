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

let emit_start () = Io.String_path.write_file start_filename ""

let emit_end () = Io.String_path.write_file end_filename ""

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

let make_callback ~f =
  (* hack to skip the first event if it's creating the temp dir *)
  let state = ref `Looking_start in
  fun t events ->
    let is_marker event filename =
      Event.kind event = File
      && Filename.basename (Event.path event) = filename
      && Event.action event = Create
    in
    let stop =
      lazy
        (Fsevents.stop t;
         Fsevents.break t)
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

let fsevents ?on_event ~cwd ~paths () =
  let on_event =
    match on_event with
    | None -> print_event ~cwd
    | Some s -> s
  in
  Fsevents.create ~paths
    ~f:(make_callback ~f:(List.iter ~f:on_event))
    ~latency:0.

let test_with_operations ?on_event ?exclusion_paths f =
  test (fun finish ->
      let cwd = Sys.getcwd () in
      let t = fsevents ?on_event ~paths:[ cwd ] ~cwd () in
      (match exclusion_paths with
      | None -> ()
      | Some f ->
        let paths = f cwd in
        Fsevents.set_exclusion_paths t ~paths);
      Fsevents.start t;
      let (_ : Thread.t) =
        Thread.create
          (fun () ->
            emit_start ();
            f ();
            emit_end ())
          ()
      in
      (match Fsevents.loop t with
      | Error Exit -> print_endline "[EXIT]"
      | Error _ -> assert false
      | Ok () -> ());
      Fsevents.destroy t;
      finish ())

let%expect_test "file create event" =
  test_with_operations (fun () -> Io.String_path.write_file "./file" "foobar");
  [%expect
    {| > { action = "Unknown"; kind = "File"; path = "$TESTCASE_ROOT/file" } |}]

let%expect_test "dir create event" =
  test_with_operations (fun () -> ignore (Fpath.mkdir "./blahblah"));
  [%expect
    {| > { action = "Create"; kind = "Dir"; path = "$TESTCASE_ROOT/blahblah" } |}]

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
    {| > { action = "Create"; kind = "Dir"; path = "$TESTCASE_ROOT/ignored" } |}];
  (* but relative paths do not *)
  run (fun _ name -> name);
  [%expect
    {|
    > { action = "Unknown"; kind = "File"; path = "$TESTCASE_ROOT/ignored/old" }
    > { action = "Create"; kind = "Dir"; path = "$TESTCASE_ROOT/ignored" } |}]

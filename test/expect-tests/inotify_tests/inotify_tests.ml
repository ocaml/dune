open Stdune
open Async_inotify_for_dune
open Printf

let ( / ) a b =
  match a with
  | "." -> b
  | _ -> Filename.concat a b
;;

let create_file fn = Io.String_path.write_file fn ""
let mkdir fn = Unix.mkdir fn 0o777
let rm = Sys.remove
let rmdir = Unix.rmdir

let string_of_event ev =
  let s = Async_inotify.Event.to_string ev in
  (* Add space padding to make events aligned *)
  let kind, rest = String.lsplit2_exn s ~on:' ' in
  sprintf "%-10s%s" kind rest
;;

let print_event ev = print_endline (string_of_event ev)
let print_events = List.iter ~f:print_event
let remove_dot_slash s = String.drop_prefix s ~prefix:"./" |> Option.value ~default:s

(* Events generated from watching directory "." are prefixed with ".". Remove
   the prefix as it's not super interesting. *)
let remove_dot_slash_from_event : Async_inotify.Event.t -> Async_inotify.Event.t
  = function
  | Created s -> Created (remove_dot_slash s)
  | Unlinked s -> Unlinked (remove_dot_slash s)
  | Modified s -> Modified (remove_dot_slash s)
  | Moved x ->
    Moved
      (match x with
       | Away s -> Away (remove_dot_slash s)
       | Into s -> Into (remove_dot_slash s)
       | Move (a, b) -> Move (remove_dot_slash a, remove_dot_slash b))
  | Queue_overflow -> Queue_overflow
;;

let watch, collect_events =
  (* Files used to mark the beginning and end of tests. *)
  let beginning_of_test_file =
    Temp.create Temp.File ~prefix:"BEGINNING_OF_TEST" ~suffix:""
  in
  let end_of_test_file = Temp.create Temp.File ~prefix:"END_OF_TEST" ~suffix:"" in
  let beginning_of_test_file = Path.to_string beginning_of_test_file in
  let end_of_test_file = Path.to_string end_of_test_file in
  create_file beginning_of_test_file;
  create_file end_of_test_file;
  let events = Queue.create () in
  let mutex = Mutex.create () in
  let cond = Condition.create () in
  let inotify =
    Async_inotify.create
      ~spawn_thread:(fun f -> ignore (Thread.create f () : Thread.t))
      ~modify_event_selector:`Closed_writable_fd
      ~log_error:print_endline
      ~send_emit_events_job_to_scheduler:(fun f ->
        Mutex.lock mutex;
        Queue.push events f;
        Condition.signal cond;
        Mutex.unlock mutex)
  in
  let watch fn = Async_inotify.add inotify fn in
  watch beginning_of_test_file;
  watch end_of_test_file;
  let next_events () =
    Mutex.lock mutex;
    while Queue.is_empty events do
      Condition.wait cond mutex
    done;
    let f = Queue.pop_exn events in
    Mutex.unlock mutex;
    f ()
  in
  let rec collect_events acc = function
    | [] ->
      let events = next_events () in
      collect_events acc events
    | Async_inotify.Event.Modified fn :: events when fn = end_of_test_file ->
      if not (List.is_empty events)
      then (
        printf "***** Leftover events after end of test marker event *****\n";
        print_events events);
      List.rev acc
    | ev :: events -> collect_events (ev :: acc) events
  in
  let collect_events () =
    (* Mark the beginning of the current test *)
    create_file end_of_test_file;
    let events =
      match next_events () with
      | [] -> assert false
      | Async_inotify.Event.Modified fn :: events when fn = beginning_of_test_file ->
        collect_events [] events
      | events ->
        printf "***** First event is not the beginning of test marker *****\n";
        collect_events [] events
    in
    (* Mark the beginning of the next test *)
    create_file beginning_of_test_file;
    events
  in
  create_file beginning_of_test_file;
  watch, collect_events
;;

(* Run a function in a sub-directory *)
let in_sub_dir =
  let n = ref 0 in
  fun f ->
    incr n;
    let dir = sprintf "test%d" !n in
    mkdir dir;
    Sys.chdir dir;
    Exn.protect ~finally:(fun () -> Sys.chdir "..") ~f
;;

let%expect_test "Simple test" =
  in_sub_dir
  @@ fun () ->
  let fn = "file" in
  create_file fn;
  watch fn;
  create_file fn;
  print_events (collect_events ());
  [%expect {| modified  file |}]
;;

let fold_int n ~init ~f =
  let rec loop i acc = if i = n then acc else loop (i + 1) (f i acc) in
  loop 0 init
;;

type kind =
  | File
  | Dir

let rec gen_tree acc ~dir ~depth ~files_per_dir ~sub_dirs_per_dir =
  let acc =
    fold_int files_per_dir ~init:acc ~f:(fun n acc ->
      let fn = dir / sprintf "f%d" (n + 1) in
      create_file fn;
      (File, fn) :: acc)
  in
  if depth = 0
  then acc
  else
    fold_int sub_dirs_per_dir ~init:acc ~f:(fun n acc ->
      let dir = dir / sprintf "d%d" (n + 1) in
      let acc = (Dir, dir) :: acc in
      mkdir dir;
      gen_tree acc ~dir ~depth:(depth - 1) ~files_per_dir ~sub_dirs_per_dir)
;;

let gen_tree ~depth ~files_per_dir ~sub_dirs_per_dir =
  List.rev (gen_tree [ Dir, "." ] ~dir:"." ~depth ~files_per_dir ~sub_dirs_per_dir)
;;

let%expect_test "Show that gen_tree generates filenames in the right order" =
  in_sub_dir
  @@ fun () ->
  List.iter (gen_tree ~depth:1 ~files_per_dir:2 ~sub_dirs_per_dir:2) ~f:(fun (_, fn) ->
    print_endline fn);
  [%expect
    {|
    .
    f1
    f2
    d1
    d1/f1
    d1/f2
    d2
    d2/f1
    d2/f2 |}]
;;

(* Return the expected set of inotify events *)
let gen_changes files =
  List.concat_map files ~f:(function
    | Dir, fn ->
      let new_file = fn / "new-file" in
      let new_dir = fn / "new-dir" in
      create_file new_file;
      mkdir new_dir;
      rmdir new_dir;
      rm new_file;
      [ Async_inotify.Event.Created new_file
      ; Modified new_file
      ; Created new_dir
      ; Unlinked new_dir
      ; Unlinked new_file
      ]
    | File, fn ->
      create_file fn;
      (* We get the event twice because we are watching both the file and the
         directory *)
      [ Modified fn; Modified fn ])
;;

let setup1 ~depth ~files_per_dir ~sub_dirs_per_dir =
  let files = gen_tree ~depth ~files_per_dir ~sub_dirs_per_dir in
  List.iter files ~f:(fun (_kind, fn) -> watch fn);
  files, collect_events
;;

let check_events ~real_events ~expected_events =
  let real_events = List.map real_events ~f:remove_dot_slash_from_event in
  print_endline (if real_events = expected_events then "Success" else "FAILURE");
  print_endline "";
  let rec loop real expected =
    match real, expected with
    | [], [] -> ()
    | ev :: real, [] ->
      printf "%s <- XXX expected no more events\n" (string_of_event ev);
      print_events real
    | [], ev :: _ -> printf "XXX expected:  %s\n" (Async_inotify.Event.to_string ev)
    | ev :: real, ev' :: expected ->
      if ev = ev'
      then (
        print_event ev;
        loop real expected)
      else (
        printf
          "%s <- XXX first mismatch, expected: %s\n"
          (string_of_event ev)
          (Async_inotify.Event.to_string ev');
        print_events real)
  in
  loop real_events expected_events
;;

let%expect_test "Check that FS events are reported chronologically" =
  in_sub_dir
  @@ fun () ->
  let files, collect_events = setup1 ~depth:2 ~files_per_dir:3 ~sub_dirs_per_dir:2 in
  let expected_events = gen_changes files in
  check_events ~expected_events ~real_events:(collect_events ());
  [%expect
    {|
    Success

    created   new-file
    modified  new-file
    created   new-dir
    unlinked  new-dir
    unlinked  new-file
    modified  f1
    modified  f1
    modified  f2
    modified  f2
    modified  f3
    modified  f3
    created   d1/new-file
    modified  d1/new-file
    created   d1/new-dir
    unlinked  d1/new-dir
    unlinked  d1/new-file
    modified  d1/f1
    modified  d1/f1
    modified  d1/f2
    modified  d1/f2
    modified  d1/f3
    modified  d1/f3
    created   d1/d1/new-file
    modified  d1/d1/new-file
    created   d1/d1/new-dir
    unlinked  d1/d1/new-dir
    unlinked  d1/d1/new-file
    modified  d1/d1/f1
    modified  d1/d1/f1
    modified  d1/d1/f2
    modified  d1/d1/f2
    modified  d1/d1/f3
    modified  d1/d1/f3
    created   d1/d2/new-file
    modified  d1/d2/new-file
    created   d1/d2/new-dir
    unlinked  d1/d2/new-dir
    unlinked  d1/d2/new-file
    modified  d1/d2/f1
    modified  d1/d2/f1
    modified  d1/d2/f2
    modified  d1/d2/f2
    modified  d1/d2/f3
    modified  d1/d2/f3
    created   d2/new-file
    modified  d2/new-file
    created   d2/new-dir
    unlinked  d2/new-dir
    unlinked  d2/new-file
    modified  d2/f1
    modified  d2/f1
    modified  d2/f2
    modified  d2/f2
    modified  d2/f3
    modified  d2/f3
    created   d2/d1/new-file
    modified  d2/d1/new-file
    created   d2/d1/new-dir
    unlinked  d2/d1/new-dir
    unlinked  d2/d1/new-file
    modified  d2/d1/f1
    modified  d2/d1/f1
    modified  d2/d1/f2
    modified  d2/d1/f2
    modified  d2/d1/f3
    modified  d2/d1/f3
    created   d2/d2/new-file
    modified  d2/d2/new-file
    created   d2/d2/new-dir
    unlinked  d2/d2/new-dir
    unlinked  d2/d2/new-file
    modified  d2/d2/f1
    modified  d2/d2/f1
    modified  d2/d2/f2
    modified  d2/d2/f2
    modified  d2/d2/f3
    modified  d2/d2/f3 |}]
;;

(* Check interleaving more specifically *)
let%expect_test "Check that FS events are reported chronologically 2" =
  in_sub_dir
  @@ fun () ->
  mkdir "a";
  mkdir "b";
  watch "a";
  watch "b";
  let expected_events = Queue.create () in
  let expect (ev : Async_inotify.Event.t) = Queue.push expected_events ev in
  let create_file fn =
    create_file fn;
    expect (Created fn);
    expect (Modified fn)
  in
  create_file "a/x";
  create_file "b/x";
  create_file "a/y";
  let expected_events = Queue.to_list expected_events in
  check_events ~expected_events ~real_events:(collect_events ());
  [%expect
    {|
    Success

    created   a/x
    modified  a/x
    created   b/x
    modified  b/x
    created   a/y
    modified  a/y |}]
;;

let run cmd =
  match
    snd
      (Unix.waitpid
         []
         (Unix.create_process
            (List.hd cmd)
            (Array.of_list cmd)
            Unix.stdin
            Unix.stdout
            Unix.stderr))
  with
  | WEXITED 0 -> ()
  | _ -> assert false
;;

(* Check that ordering is respected when the changes are made by an external
   process. Which is the assumption we are making for the fs sync mechanism of
   the file watcher. *)
let%expect_test "Check that FS events are reported chronologically 3" =
  in_sub_dir
  @@ fun () ->
  mkdir "_build";
  mkdir "_build/.sync";
  watch ".";
  watch "_build/.sync";
  let actions =
    [ `Me; `Ext; `Me; `Ext; `Ext ] |> List.mapi ~f:(fun i who -> string_of_int i, who)
  in
  let actions = actions @ [ "_build/.sync/1", `Me ] in
  let do_actions () =
    List.iter actions ~f:(fun (fn, who) ->
      match who with
      | `Me -> create_file fn
      | `Ext -> run [ "touch"; fn ])
  in
  do_actions ();
  let expected_events =
    List.concat_map actions ~f:(fun (fn, _who) ->
      [ Async_inotify.Event.Created fn; Modified fn ])
  in
  check_events ~expected_events ~real_events:(collect_events ());
  [%expect
    {|
    Success

    created   0
    modified  0
    created   1
    modified  1
    created   2
    modified  2
    created   3
    modified  3
    created   4
    modified  4
    created   _build/.sync/1
    modified  _build/.sync/1 |}];
  (* Repeat the operation multiple times *)
  for _ = 0 to 100 do
    List.iter actions ~f:(fun (fn, _) -> rm fn);
    ignore (collect_events () : _ list);
    do_actions ();
    let real_events = collect_events () |> List.map ~f:remove_dot_slash_from_event in
    if expected_events <> real_events
    then (
      print_endline "--------------------";
      check_events ~real_events ~expected_events)
  done
;;

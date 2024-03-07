open Stdune

let remove_dot_slash s = String.drop_prefix s ~prefix:".\\" |> Option.value ~default:s
let create_file fn = Io.String_path.write_file fn ""
let mkdir fn = Unix.mkdir fn 0o777

type event =
  { action : string
  ; path : string
  }

let dyn_of_event ev =
  let action =
    match Fswatch_win.Event.action ev with
    | Added -> "added"
    | Removed -> "removed"
    | Modified -> "modified"
    | Renamed_old -> "renamed_old"
    | Renamed_new -> "renamed_new"
  in
  let path = remove_dot_slash (Fswatch_win.Event.path ev) in
  Dyn.record [ "action", Dyn.string action; "path", Dyn.string path ]
;;

let dyn_of_event' { action; path } =
  let path = remove_dot_slash path in
  Dyn.record [ "action", Dyn.string action; "path", Dyn.string path ]
;;

let print_events events = print_endline (Dyn.to_string (Dyn.list Fun.id events))
let markdir = Filename.concat (Sys.getcwd ()) "mark"
let beginning_of_test = "BEGINNING_OF_TEST"
let end_of_test = "END_OF_TEST"

let watch, collect_events =
  (* File used to mark the beginning and end of tests. *)
  mkdir markdir;
  let beginning_of_test_file = Filename.concat markdir beginning_of_test in
  let end_of_test_file = Filename.concat markdir end_of_test in
  create_file beginning_of_test_file;
  create_file end_of_test_file;
  let fswatch = Fswatch_win.create () in
  let watch dir =
    let dir =
      if Filename.is_relative dir then Filename.concat (Sys.getcwd ()) dir else dir
    in
    Fswatch_win.add fswatch dir
  in
  watch markdir;
  let rec collect_events acc = function
    | [] ->
      let events = Fswatch_win.wait fswatch ~sleep:0 in
      collect_events acc events
    | e :: events when Fswatch_win.Event.path e = end_of_test ->
      if not (List.is_empty events)
      then (
        Printf.printf "***** Leftover events after end of test marker event *****\n";
        print_events (List.map ~f:dyn_of_event events));
      List.rev_map ~f:dyn_of_event acc
    | ev :: events -> collect_events (ev :: acc) events
  in
  let collect_events () =
    (* Mark the beginning of the current test *)
    create_file end_of_test_file;
    let events =
      let events = Fswatch_win.wait fswatch ~sleep:0 in
      (* List.iter ~f:(fun ev -> print_endline (Dyn.to_string (Fswatch_win.Event.to_dyn ev))) events; *)
      match events with
      | [] -> assert false
      | e :: events when Fswatch_win.Event.path e = beginning_of_test ->
        collect_events [] events
      | events ->
        Printf.printf "***** First event is not the beginning of test marker *****\n";
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
    let dir = Printf.sprintf "test%d" !n in
    mkdir dir;
    Sys.chdir dir;
    Exn.protect ~finally:(fun () -> Sys.chdir "..") ~f
;;

let check_events ~real_events expected_events =
  let expected_events = List.map ~f:dyn_of_event' expected_events in
  if real_events = expected_events
  then ()
  else (
    print_endline "** FAILURE **";
    print_endline "ACTUAL:";
    print_events real_events;
    print_endline "EXPECTED:";
    print_events expected_events;
    exit 1)
;;

let _ =
  in_sub_dir
  @@ fun () ->
  let fn = "file" in
  create_file fn;
  watch ".";
  create_file fn;
  check_events ~real_events:(collect_events ()) [ { action = "modified"; path = fn } ]
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
      let fn = Filename.concat dir (Printf.sprintf "f%d" (n + 1)) in
      create_file fn;
      (File, fn) :: acc)
  in
  if depth = 0
  then acc
  else
    fold_int sub_dirs_per_dir ~init:acc ~f:(fun n acc ->
      let dir = Filename.concat dir (Printf.sprintf "d%d" (n + 1)) in
      let acc = (Dir, dir) :: acc in
      mkdir dir;
      gen_tree acc ~dir ~depth:(depth - 1) ~files_per_dir ~sub_dirs_per_dir)
;;

let gen_tree ~depth ~files_per_dir ~sub_dirs_per_dir =
  List.rev (gen_tree [ Dir, "." ] ~dir:"." ~depth ~files_per_dir ~sub_dirs_per_dir)
;;

let _ =
  (* Show that gen_tree generates filenames in the right order *)
  in_sub_dir
  @@ fun () ->
  let entries =
    List.map (gen_tree ~depth:1 ~files_per_dir:2 ~sub_dirs_per_dir:2) ~f:Stdlib.snd
  in
  (* List.iter ~f:print_endline entries; *)
  assert (
    List.map ~f:remove_dot_slash entries
    = [ "."; "f1"; "f2"; "d1"; "d1\\f1"; "d1\\f2"; "d2"; "d2\\f1"; "d2\\f2" ])
;;

(* Return the expected set of inotify events *)
let gen_changes files =
  List.iter files ~f:(function
    | Dir, fn ->
      let new_file = Filename.concat fn "new-file" in
      let new_dir = Filename.concat fn "new-dir" in
      create_file new_file;
      mkdir new_dir;
      Unix.rmdir new_dir;
      Sys.remove new_file
    | File, fn -> create_file fn)
;;

let setup1 ~depth ~files_per_dir ~sub_dirs_per_dir =
  let files = gen_tree ~depth ~files_per_dir ~sub_dirs_per_dir in
  watch ".";
  files, collect_events
;;

let _ =
  (* Check that FS events are reported chronologically *)
  in_sub_dir
  @@ fun () ->
  let files, collect_events = setup1 ~depth:2 ~files_per_dir:3 ~sub_dirs_per_dir:2 in
  gen_changes files;
  check_events
    ~real_events:(collect_events ())
    [ { action = "added"; path = "new-file" }
    ; { action = "added"; path = "new-dir" }
    ; { action = "removed"; path = "new-dir" }
    ; { action = "removed"; path = "new-file" }
    ; { action = "modified"; path = "f1" }
    ; { action = "modified"; path = "f2" }
    ; { action = "modified"; path = "f3" }
    ; { action = "added"; path = "d1\\new-file" }
    ; { action = "added"; path = "d1\\new-dir" }
    ; { action = "removed"; path = "d1\\new-dir" }
    ; { action = "removed"; path = "d1\\new-file" }
    ; { action = "modified"; path = "d1\\f1" }
    ; { action = "modified"; path = "d1\\f2" }
    ; { action = "modified"; path = "d1\\f3" }
    ; { action = "added"; path = "d1\\d1\\new-file" }
    ; { action = "added"; path = "d1\\d1\\new-dir" }
    ; { action = "removed"; path = "d1\\d1\\new-dir" }
    ; { action = "removed"; path = "d1\\d1\\new-file" }
    ; { action = "modified"; path = "d1\\d1\\f1" }
    ; { action = "modified"; path = "d1\\d1\\f2" }
    ; { action = "modified"; path = "d1\\d1\\f3" }
    ; { action = "added"; path = "d1\\d2\\new-file" }
    ; { action = "added"; path = "d1\\d2\\new-dir" }
    ; { action = "removed"; path = "d1\\d2\\new-dir" }
    ; { action = "removed"; path = "d1\\d2\\new-file" }
    ; { action = "modified"; path = "d1\\d2\\f1" }
    ; { action = "modified"; path = "d1\\d2\\f2" }
    ; { action = "modified"; path = "d1\\d2\\f3" }
    ; { action = "added"; path = "d2\\new-file" }
    ; { action = "added"; path = "d2\\new-dir" }
    ; { action = "removed"; path = "d2\\new-dir" }
    ; { action = "removed"; path = "d2\\new-file" }
    ; { action = "modified"; path = "d2\\f1" }
    ; { action = "modified"; path = "d2\\f2" }
    ; { action = "modified"; path = "d2\\f3" }
    ; { action = "added"; path = "d2\\d1\\new-file" }
    ; { action = "added"; path = "d2\\d1\\new-dir" }
    ; { action = "removed"; path = "d2\\d1\\new-dir" }
    ; { action = "removed"; path = "d2\\d1\\new-file" }
    ; { action = "modified"; path = "d2\\d1\\f1" }
    ; { action = "modified"; path = "d2\\d1\\f2" }
    ; { action = "modified"; path = "d2\\d1\\f3" }
    ; { action = "added"; path = "d2\\d2\\new-file" }
    ; { action = "added"; path = "d2\\d2\\new-dir" }
    ; { action = "removed"; path = "d2\\d2\\new-dir" }
    ; { action = "removed"; path = "d2\\d2\\new-file" }
    ; { action = "modified"; path = "d2\\d2\\f1" }
    ; { action = "modified"; path = "d2\\d2\\f2" }
    ; { action = "modified"; path = "d2\\d2\\f3" }
    ]
;;

(* Check interleaving more specifically *)
let _ =
  in_sub_dir
  @@ fun () ->
  mkdir "a";
  mkdir "b";
  watch ".";
  create_file "a\\x";
  create_file "b\\x";
  create_file "a\\y";
  check_events
    ~real_events:(collect_events ())
    [ { action = "added"; path = "a\\x" }
    ; { action = "added"; path = "b\\x" }
    ; { action = "added"; path = "a\\y" }
    ]
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
let _ =
  in_sub_dir
  @@ fun () ->
  mkdir "_build";
  mkdir "_build\\.sync";
  watch ".";
  let actions =
    [ `Me; `Ext; `Me; `Ext; `Ext ] |> List.mapi ~f:(fun i who -> string_of_int i, who)
  in
  let actions = actions @ [ "_build\\.sync\\1", `Me ] in
  let do_actions () =
    List.iter actions ~f:(fun (fn, who) ->
      match who with
      | `Me -> create_file fn
      | `Ext -> run [ "touch"; fn ])
  in
  do_actions ();
  let real_events = collect_events () in
  let expected_events =
    [ { action = "added"; path = "0" }
    ; { action = "added"; path = "1" }
    ; { action = "modified"; path = "1" }
    ; { action = "added"; path = "2" }
    ; { action = "added"; path = "3" }
    ; { action = "modified"; path = "3" }
    ; { action = "added"; path = "4" }
    ; { action = "modified"; path = "4" }
    ; { action = "added"; path = "_build\\.sync\\1" }
    ]
  in
  check_events ~real_events expected_events
;;

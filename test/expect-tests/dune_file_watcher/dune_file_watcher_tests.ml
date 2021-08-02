let printf = Printf.printf

open Stdune
open Base

let tmp_dir = Stdlib.Filename.concat (Unix.getcwd ()) "working-dir"

let () =
  try Unix.mkdir tmp_dir 0o777 with
  | _ -> ()

let () = Unix.chdir tmp_dir

let () = Path.set_root (Path.External.of_string tmp_dir)

let () = Path.Build.set_build_dir (Path.Build.Kind.of_string "_build")

let now () = Unix.gettimeofday ()

let retry_loop (type a) ~period ~timeout ~(f : unit -> a option) : a option =
  let t0 = now () in
  let rec loop () =
    match f () with
    | Some res -> Some res
    | None ->
      let t1 = now () in
      if Float.( < ) (t1 -. t0) timeout then (
        Thread.delay period;
        loop ()
      ) else
        None
  in
  loop ()

let print_s sexp = printf "%s\n" (Sexp.to_string_hum sexp)

let sexp_of_path path = sexp_of_string (Path.to_string path)

let get_events ~try_to_get_events ~expected =
  let collected = ref [] in
  let done_collecting =
    match expected with
    | 0 -> Some `Enough
    | n ->
      assert (n > 0);
      retry_loop ~period:0.01 ~timeout:3.0 ~f:(fun () ->
          match try_to_get_events () with
          | None -> None
          | Some events ->
            collected := !collected @ events;
            if List.length !collected >= expected then
              Some `Enough
            else
              None)
  in
  match done_collecting with
  | None -> (!collected, `Not_enough)
  | Some `Enough ->
    Thread.delay 0.02;
    (match try_to_get_events () with
    | Some events -> collected := !collected @ events
    | None -> ());
    if List.length !collected > expected then
      (!collected, `Too_many)
    else
      (!collected, `Ok)

let print_events ~try_to_get_events ~expected =
  let events, status = get_events ~try_to_get_events ~expected in
  List.iter events ~f:(fun event -> print_s ([%sexp_of: path] event));
  match status with
  | `Ok -> ()
  | `Not_enough ->
    printf "Timed out waiting for more events: expected %d, saw %d\n" expected
      (List.length events)
  | `Too_many ->
    printf "Got more events than expected: expected %d, saw %d\n" expected
      (List.length events)

let%expect_test _ =
  let mutex = Mutex.create () in
  let critical_section ~f =
    Mutex.lock mutex;
    Exn.protect ~f ~finally:(fun () -> Mutex.unlock mutex)
  in
  let events_buffer = ref [] in
  let watcher =
    Dune_file_watcher.create_external ~debounce_interval:None
      ~scheduler:
        { spawn_thread = (fun f -> ignore (Thread.create f () : Thread.t))
        ; thread_safe_send_emit_events_job =
            (fun job ->
              Mutex.lock mutex;
              let events = job () in
              events_buffer := !events_buffer @ events;
              Mutex.unlock mutex)
        }
      ~root:Path.root
  in
  let try_to_get_events () =
    critical_section ~f:(fun () ->
        match !events_buffer with
        | [] -> None
        | list ->
          events_buffer := [];
          Some
            (List.map list ~f:(function
              | Dune_file_watcher.Event.Sync -> assert false
              | Dune_file_watcher.Event.Queue_overflow -> assert false
              | Dune_file_watcher.Event.Fs_memo_event { path; kind = _ } -> path
              | Dune_file_watcher.Event.Watcher_terminated -> assert false)))
  in
  let print_events n = print_events ~try_to_get_events ~expected:n in
  Dune_file_watcher.wait_for_initial_watches_established_blocking watcher;
  Stdio.Out_channel.write_all "x" ~data:"x";
  print_events 1;
  [%expect {|
x
|}];
  (* CR-someday aalekseyev: renaming is not detected *)
  Unix.rename "x" "y";
  print_events 0;
  [%expect {|
|}];
  Dune_file_watcher.For_tests.suspend watcher;
  let (_ : _) = Fpath.mkdir_p "d/w" in
  Stdio.Out_channel.write_all "d/w/x" ~data:"x";
  Dune_file_watcher.For_tests.resume watcher;
  (* CR-someday aalekseyev: race: [x] is created too quickly for inotifywait to
     notice it. We suspend&resume the watcher so that the race reliably goes in
     our favor, but the bug can be reproduced even if we don't do that. *)
  print_events 0;
  [%expect {|
|}];
  (* CR-someday aalekseyev: also, mkdir is not reported *)
  Stdio.Out_channel.write_all "d/w/y" ~data:"y";
  (* unlike [x] above, the creation of [y] is noticed even though the only
     difference between them is creation time *)
  print_events 1;
  [%expect {|
  d/w/y
|}];
  (* CR-someday aalekseyev: Here I tried to reproduce a bug
     [https://github.com/inotify-tools/inotify-tools/issues/130], but I couldn't
     quite reproduce it (even though it reproduces in terminal).

     The current behavior still seems buggy, in a different way, and is not
     deterministic, so I'm leaving the test disabled for now. *)
  if false then (
    let (_ : _) = Fpath.mkdir_p "d/a1/a2/a3/a4/a5/a6/a7/a8/a9/a10" in
    let d_w = Unix.openfile "d/a1/a2/a3/a4/a5/a6/a7/a8/a9/a10" [] 0 in
    Unix.rename "d" "d2";
    print_events 0;
    let () =
      Spawn.spawn ~cwd:(Fd d_w) ~prog:"/usr/bin/env"
        ~argv:[ "env"; "touch"; "z" ] ()
      |> Unix.waitpid []
      |> function
      | _, (WEXITED 0 : Unix.process_status) -> ()
      | _, _ -> failwith "[touch] failed"
    in
    print_events 1;
    Stdio.Out_channel.write_all "d2/a1/a2/a3/a4/a5/a6/a7/a8/a9/a10/y" ~data:"y";
    print_events 1
  );
  [%expect {|
|}]

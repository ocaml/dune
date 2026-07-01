let printf = Printf.printf

open Base
open Stdune

let init () =
  let tmp_dir = Stdlib.Filename.concat (Unix.getcwd ()) "working-dir" in
  let () =
    try Unix.mkdir tmp_dir 0o777 with
    | _ -> ()
  in
  Unix.chdir tmp_dir;
  Path.set_root (Path.External.of_string tmp_dir);
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build")
;;

let create_watcher ?fsevents_debounce ~watch_exclusions () =
  let mutex = Mutex.create () in
  let events_buffer = ref [] in
  let try_to_get_events () =
    Mutex.protect mutex (fun () ->
      match !events_buffer with
      | [] -> None
      | events ->
        events_buffer := [];
        Some events)
  in
  let event_queue = Dune_scheduler.Event.Queue.create () in
  let watcher =
    Dune_scheduler.File_watcher.create
      ?fsevents_debounce
      ~watch_exclusions
      ~event_queue
      ()
  in
  let rec read_events () =
    let open Fiber.O in
    Dune_scheduler.File_watcher.read watcher
    >>= function
    | None -> Fiber.return ()
    | Some events ->
      let events =
        List.map events ~f:(function
          | Dune_scheduler.Event.File_watcher_event.Fs_memo_event event -> event
          | Queue_overflow -> assert false)
      in
      Mutex.protect mutex (fun () -> events_buffer := !events_buffer @ events);
      read_events ()
  in
  let (_ : Thread.t) =
    let iter () =
      match Dune_scheduler.Event.Queue.next event_queue with
      | Fiber_fill_ivar fill -> [ fill ]
      | Shutdown _ | Job_complete_ready -> assert false
    in
    Thread.create (fun () -> Fiber.run (read_events ()) ~iter) ()
  in
  watcher, try_to_get_events
;;

let retry_loop (type a) ~period ~timeout ~(f : unit -> a option) : a option =
  let t0 = Time.now () in
  let rec loop () =
    match f () with
    | Some res -> Some res
    | None ->
      let t1 = Time.now () in
      if Base.Float.( < ) (Time.Span.to_secs (Time.diff t1 t0)) timeout
      then (
        Thread.delay period;
        loop ())
      else None
  in
  loop ()
;;

let get_events ~try_to_get_events ~expected =
  let collected = ref [] in
  let done_collecting =
    match expected with
    | 0 -> Some `Enough
    | n ->
      assert (n > 0);
      retry_loop ~period:0.01 ~timeout:3.0 ~f:(fun () ->
        let open Option.O in
        let* events = try_to_get_events () in
        collected := !collected @ events;
        if List.length !collected >= expected then Some `Enough else None)
  in
  match done_collecting with
  | None -> !collected, `Not_enough
  | Some `Enough ->
    Thread.delay 0.02;
    (match try_to_get_events () with
     | Some events -> collected := !collected @ events
     | None -> ());
    !collected, if List.length !collected > expected then `Too_many else `Ok
;;

let print_events ~try_to_get_events ~expected =
  let events, status = get_events ~try_to_get_events ~expected in
  List.iter events ~f:(fun event ->
    Dune_scheduler.Event.Fs_memo_event.to_dyn event
    |> Dyn.to_string
    |> Stdio.print_endline);
  match status with
  | `Ok -> ()
  | `Not_enough ->
    printf
      "Timed out waiting for more events: expected %d, saw %d\n"
      expected
      (List.length events)
  | `Too_many ->
    printf
      "Got more events than expected: expected %d, saw %d\n"
      expected
      (List.length events)
;;

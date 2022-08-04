let printf = Printf.printf

open Base
open Stdune

let critical_section mutex ~f =
  Mutex.lock mutex;
  Exn.protect ~f ~finally:(fun () -> Mutex.unlock mutex)

let init () =
  let tmp_dir = Stdlib.Filename.concat (Unix.getcwd ()) "working-dir" in
  let () = try Unix.mkdir tmp_dir 0o777 with _ -> () in
  Unix.chdir tmp_dir;
  Path.set_root (Path.External.of_string tmp_dir);
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build")

let now () = Unix.gettimeofday ()

let retry_loop (type a) ~period ~timeout ~(f : unit -> a option) : a option =
  let t0 = now () in
  let rec loop () =
    match f () with
    | Some res -> Some res
    | None ->
      let t1 = now () in
      if Base.Float.( < ) (t1 -. t0) timeout then (
        Thread.delay period;
        loop ())
      else None
  in
  loop ()

let get_events ~try_to_get_events ~expected =
  let collected = ref [] in
  let done_collecting =
    match expected with
    | 0 -> Some `Enough
    | n ->
      assert (n > 0);
      retry_loop ~period:0.01 ~timeout:3.0 ~f:(fun () ->
          let open Option.O in
          try_to_get_events () >>= fun events ->
          collected := !collected @ events;
          if List.length !collected >= expected then Some `Enough else None)
  in
  match done_collecting with
  | None -> (!collected, `Not_enough)
  | Some `Enough ->
    Thread.delay 0.02;
    (match try_to_get_events () with
    | Some events -> collected := !collected @ events
    | None -> ());
    (!collected, if List.length !collected > expected then `Too_many else `Ok)

let print_events ~try_to_get_events ~expected =
  let events, status = get_events ~try_to_get_events ~expected in
  List.iter events ~f:(fun event ->
      Dune_file_watcher.Fs_memo_event.to_dyn event
      |> Dyn.to_string |> Stdio.print_endline);
  match status with
  | `Ok -> ()
  | `Not_enough ->
    printf "Timed out waiting for more events: expected %d, saw %d\n" expected
      (List.length events)
  | `Too_many ->
    printf "Got more events than expected: expected %d, saw %d\n" expected
      (List.length events)

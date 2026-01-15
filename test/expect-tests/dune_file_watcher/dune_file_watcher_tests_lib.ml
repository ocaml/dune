let printf = Printf.printf

open Base
open Stdune

let critical_section mutex ~f =
  (* Since 5.0, using "Mutex" with Base open rings an alert and suggests
     we use "Stdlib.Mutex" instead.
     Prior to OCaml 5.0, "Stdlib.Mutex" didn't exist, it was just "Mutex".
     Since 5.1 there is Stdlib.Mutex.protect which replaces this function.
  *)
  let module Mutex = Mutex [@alert "-deprecated"] in
  Mutex.lock mutex;
  Exn.protect ~f ~finally:(fun () -> Mutex.unlock mutex)
;;

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
    Dune_file_watcher.Fs_memo_event.to_dyn event |> Dyn.to_string |> Stdio.print_endline);
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

let path_of_event (event : Dune_file_watcher.Fs_memo_event.t) =
  match event.path with
  | In_source_tree p -> Path.Source.to_string p
  | In_build_dir p -> Path.Build.to_string p
  | External p -> Path.External.to_string p
;;

let wait_for_paths ~try_to_get_events ~paths =
  let paths_set = String.Set.of_list paths in
  let seen = ref String.Set.empty in
  let all_seen () = String.Set.is_subset paths_set ~of_:!seen in
  let result =
    retry_loop ~period:0.01 ~timeout:3.0 ~f:(fun () ->
      match try_to_get_events () with
      | None -> if all_seen () then Some () else None
      | Some events ->
        List.iter events ~f:(fun event ->
          let p = path_of_event event in
          if String.Set.mem paths_set p then seen := String.Set.add !seen p);
        if all_seen () then Some () else None)
  in
  let seen_list = String.Set.to_list !seen in
  match result with
  | Some () -> printf "saw: %s\n" (String.concat ~sep:", " seen_list)
  | None ->
    let missing = String.Set.diff paths_set !seen |> String.Set.to_list in
    printf
      "saw: %s (missing: %s)\n"
      (String.concat ~sep:", " seen_list)
      (String.concat ~sep:", " missing)
;;

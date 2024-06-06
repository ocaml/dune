open! Stdune
module Flock = Dune_util.Flock
module Scheduler = Dune_engine.Scheduler

type t =
  { flock : Flock.t
  ; lock_path : Path.t
  }

(* Global mutable set of names, used to prevent printing "waiting for
   lock" messages multiple times when multiple concurrent fibers try
   to take a lock at the same time. *)
module Global_waiting_names = struct
  let state = lazy (String.Table.create 1)

  (* add a name to the set, returning [true] iff the name wasn't
     already in the set *)
  let add name =
    let state = Lazy.force state in
    String.Table.add state name () |> Result.is_ok
  ;;

  let remove name =
    let state = Lazy.force state in
    String.Table.remove state name
  ;;
end

let attempt_to_lock { flock; lock_path } ~name_for_messages ~timeout_s =
  let open Fiber.O in
  let current_dune_pid = Unix.getpid () in
  let rec loop timeout_s =
    match Flock.lock_non_block flock Flock.Exclusive with
    | Error e -> Fiber.return @@ Error e
    | Ok `Success ->
      Global_waiting_names.remove name_for_messages;
      Fiber.return (Ok `Success)
    | Ok `Failure -> handle_failure timeout_s
  and handle_failure timeout_s =
    let locked_by_pid = int_of_string (Io.read_file lock_path) in
    let sleep_duration_s = 0.1 in
    let remaining_duration_s = timeout_s -. sleep_duration_s in
    if remaining_duration_s <= 0.0
    then Fiber.return (Ok `Timeout)
    else (
      if locked_by_pid <> current_dune_pid && Global_waiting_names.add name_for_messages
      then
        (* Only print this message if the dune process that holds the
           lock isn't the current process and this is the first time
           that the current process has failed to take the lock since
           the last time it successfully took the lock. This prevents
           the situation where multiple fibers all attempt to take the
           lock concurrently while it's held by another process from
           causing the following message from being printed multiple
           times. *)
        User_message.print
          (User_message.make
             [ Pp.textf
                 "Waiting for another instance of dune (pid %d) to release the lock for \
                  the resource %S..."
                 locked_by_pid
                 name_for_messages
             ]);
      let* () = Scheduler.sleep sleep_duration_s in
      loop remaining_duration_s)
  in
  loop timeout_s
;;

let with_flock lock_path ~name_for_messages ~timeout_s ~f =
  let open Fiber.O in
  let parent = Path.parent_exn lock_path in
  Path.mkdir_p parent;
  let fd =
    Unix.openfile
      (Path.to_string lock_path)
      [ Unix.O_CREAT; O_WRONLY; O_SHARE_DELETE; O_CLOEXEC ]
      0o600
  in
  let flock = Flock.create fd in
  let current_dune_pid = Unix.getpid () in
  Fiber.finalize
    ~finally:(fun () ->
      let+ () = Fiber.return () in
      Unix.close fd)
    (fun () ->
      attempt_to_lock { flock; lock_path } ~name_for_messages ~timeout_s
      >>= function
      | Ok `Success ->
        Fiber.finalize
          (fun () ->
            Dune_util.Global_lock.write_pid fd;
            f ())
          ~finally:(fun () ->
            let+ () = Fiber.return () in
            match Flock.unlock flock with
            | Ok () ->
              (* Note that after the lock has been released, we
                 deliberately don't delete the lock file to avoid a race
                 condition where other processes or fibers still need to
                 read the file to determine the process that held the
                 lock. Even though the lock has been released, other
                 parties may be in between timing out waiting for the
                 lock and reading the lock file to get the pid to
                 include in their error message. *)
              ()
            | Error ue ->
              Unix_error.Detailed.create ue ~syscall:"flock" ~arg:"unlock"
              |> Unix_error.Detailed.raise)
      | Ok `Timeout ->
        let locked_by_pid = int_of_string (Io.read_file lock_path) in
        if locked_by_pid == current_dune_pid
        then
          Code_error.raise
            "timeout while waiting for flock, but flock was currently held by the \
             current process"
            [ "name_for_messages", Dyn.string name_for_messages ]
        else
          User_error.raise
            ~hints:
              [ Pp.textf
                  "Another instance of dune (pid %d) currently holds the lock for the \
                   resource %S. If this is unexpected, terminate that process and re-run \
                   the command."
                  locked_by_pid
                  name_for_messages
              ; Pp.textf
                  "As a last resort, if the other instance of dune (pid %d) is no longer \
                   running, manually delete the lock file %s."
                  locked_by_pid
                  (Path.to_string_maybe_quoted lock_path)
              ]
            [ Pp.textf
                "Timed out after %.2f seconds while waiting for another instance of dune \
                 (pid %d) to release the lock on the resource %S."
                timeout_s
                locked_by_pid
                name_for_messages
            ]
      | Error error ->
        User_error.raise
          [ Pp.textf
              "Failed to get a lock for the resource %S with lock file %s: %s"
              name_for_messages
              (Path.to_string_maybe_quoted lock_path)
              (Unix.error_message error)
          ])
;;

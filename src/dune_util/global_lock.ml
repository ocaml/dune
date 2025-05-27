open Stdune
module Config = Dune_config.Config

let lock_file = Path.Build.(relative root ".lock")

let with_timeout ~timeout f =
  let now () = Unix.gettimeofday () in
  let deadline = now () +. timeout in
  let rec loop () =
    if now () >= deadline
    then `Timed_out
    else (
      match f () with
      | `Continue -> loop ()
      | `Stop -> `Success)
  in
  loop ()
;;

let write_pid fd =
  let pid = Int.to_string (Unix.getpid ()) in
  let len = String.length pid in
  let res = Unix.write fd (Bytes.of_string pid) 0 len in
  assert (res = len)
;;

module Lock = struct
  let t =
    lazy
      (Path.ensure_build_dir_exists ();
       let fd =
         Unix.openfile
           (Path.Build.to_string lock_file)
           [ Unix.O_CREAT; O_WRONLY; O_SHARE_DELETE; O_CLOEXEC ]
           0o600
       in
       Flock.create fd)
  ;;

  let or_raise_unix ~name = function
    | Ok s -> s
    | Error error ->
      Code_error.raise
        "lock"
        [ "name", Dyn.string name; "error", Dyn.string (Unix.error_message error) ]
  ;;

  let lock () =
    let t = Lazy.force t in
    let res = Flock.lock_non_block t Exclusive |> or_raise_unix ~name:"lock" in
    (match res with
     | `Failure -> ()
     | `Success ->
       let fd = Flock.fd t in
       Unix.ftruncate fd 0;
       write_pid fd);
    res
  ;;

  let unlock () =
    let lock = Lazy.force t in
    Unix.ftruncate (Flock.fd lock) 0;
    Flock.unlock lock |> or_raise_unix ~name:"unlock"
  ;;
end

let locked = ref false

module Lock_held_by = struct
  type t =
    | Pid_from_lockfile of int
    | Unknown

  let read_lock_file () =
    match Io.read_file (Path.build lock_file) with
    | exception _ -> Unknown
    | pid ->
      (match int_of_string_opt pid with
       | Some pid -> Pid_from_lockfile pid
       | None ->
         User_error.raise
           [ Pp.textf
               "Unexpected contents of build directory global lock file (%s). Expected \
                an integer PID. Found: %s"
               (Path.Build.to_string_maybe_quoted lock_file)
               pid
           ]
           ~hints:
             [ Pp.textf "Try deleting %s" (Path.Build.to_string_maybe_quoted lock_file) ])
  ;;
end

let lock ~timeout =
  match Config.(get global_lock) with
  | `Disabled -> Ok ()
  | `Enabled ->
    if !locked
    then Ok ()
    else (
      let res =
        match timeout with
        | None -> Lock.lock ()
        | Some timeout ->
          (match
             with_timeout ~timeout (fun () ->
               match Lock.lock () with
               | `Success -> `Stop
               | `Failure -> `Continue)
           with
           | `Timed_out -> `Failure
           | `Success -> `Success)
      in
      match res with
      | `Success ->
        locked := true;
        Ok ()
      | `Failure ->
        let lock_held_by = Lock_held_by.read_lock_file () in
        Error lock_held_by)
;;

let lock_exn ~timeout =
  match lock ~timeout with
  | Ok () -> ()
  | Error lock_held_by ->
    User_error.raise
      [ Pp.textf
          "A running dune%s instance has locked the build directory. If this is not the \
           case, please delete %S."
          (match lock_held_by with
           | Unknown -> ""
           | Pid_from_lockfile pid -> sprintf " (pid: %d)" pid)
          (Path.Build.to_string_maybe_quoted lock_file)
      ]
;;

let unlock () =
  if !locked
  then (
    Lock.unlock ();
    locked := false)
;;

let () = at_exit unlock

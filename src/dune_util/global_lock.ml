open Stdune

let lock_file = Path.Build.(relative root ".lock")

let with_timeout ~timeout f =
  let now () = Unix.gettimeofday () in
  let deadline = now () +. timeout in
  let rec loop () =
    if now () >= deadline then `Timed_out
    else
      match f () with
      | `Continue -> loop ()
      | `Stop -> `Success
  in
  loop ()

module type S = sig
  val lock : unit -> [ `Success | `Failure ]

  val unlock : unit -> unit
end

let write_pid fd =
  let pid = Int.to_string (Unix.getpid ()) in
  let len = String.length pid in
  let res = Unix.write fd (Bytes.of_string pid) 0 len in
  assert (res = len)

module Win () : S = struct
  let t = ref None

  let create () =
    Path.ensure_build_dir_exists ();
    match
      Unix.openfile
        (Path.Build.to_string lock_file)
        [ O_CREAT; O_EXCL; O_WRONLY ]
        0o600
    with
    | exception _ -> None
    | fd ->
      Unix.set_close_on_exec fd;
      write_pid fd;
      Some fd

  let () =
    at_exit (fun () ->
        match !t with
        | None -> ()
        | Some fd ->
          Unix.close fd;
          Path.rm_rf (Path.build lock_file))

  let lock () =
    match !t with
    | Some _ -> `Success
    | None -> (
      match create () with
      | None -> `Failure
      | Some fd ->
        t := Some fd;
        `Success)

  let unlock () =
    match !t with
    | None -> ()
    | Some fd ->
      Unix.close fd;
      Path.rm_rf (Path.build lock_file)
end

module Unix () : S = struct
  let t =
    lazy
      (Path.ensure_build_dir_exists ();
       let fd =
         Unix.openfile
           (Path.Build.to_string lock_file)
           [ Unix.O_CREAT; O_WRONLY ] 0o600
       in
       Unix.set_close_on_exec fd;
       Flock.create fd)

  let or_raise_unix ~name = function
    | Ok s -> s
    | Error _unix -> Code_error.raise "lock" [ ("name", Dyn.string name) ]

  let lock () =
    let t = Lazy.force t in
    let res = Flock.lock_non_block t Exclusive |> or_raise_unix ~name:"lock" in
    (match res with
    | `Failure -> ()
    | `Success ->
      let fd = Flock.fd t in
      write_pid fd);
    res

  let unlock () = Lazy.force t |> Flock.unlock |> or_raise_unix ~name:"unlock"
end

module Lock = (val if Sys.win32 then (module Win ()) else (module Unix ()) : S)

let locked = ref false

let lock_exn ~timeout =
  if not !locked then
    let res =
      match timeout with
      | None -> Lock.lock ()
      | Some timeout -> (
        match
          with_timeout ~timeout (fun () ->
              match Lock.lock () with
              | `Success -> `Stop
              | `Failure -> `Continue)
        with
        | `Timed_out -> `Failure
        | `Success -> `Success)
    in
    match res with
    | `Success -> locked := true
    | `Failure ->
      User_error.raise
        [ Pp.textf
            "A running dune%s instance has locked the build directory. If this \
             is not the case, please delete %s"
            (match Io.read_file (Path.build lock_file) with
            | exception _ -> ""
            | pid -> sprintf " (pid: %s)" pid)
            (Path.Build.to_string_maybe_quoted lock_file)
        ]

let unlock () =
  if !locked then (
    Lock.unlock ();
    locked := false)

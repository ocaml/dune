module Working_dir = struct
  type 'a gen =
    | Path of string
    | Fd of 'a
    | Inherit

  type t = Fd.t gen
  type raw = Unix.file_descr gen

  let raw : t -> raw = function
    | Fd fd -> Fd (Fd.unsafe_to_unix_file_descr fd)
    | (Path _ | Inherit) as x -> x
  ;;

  let path s = Path s
  let fd fd = Fd fd
  let inherit_ = Inherit
end

module Landlock = struct
  module Raw = struct
    external abi_version : unit -> int = "dune_spawn_landlock_abi_version"

    external write_access_rights
      :  int
      -> int64
      = "dune_spawn_landlock_write_access_rights"

    external create_ruleset
      :  int64
      -> Unix.file_descr
      = "dune_spawn_landlock_create_ruleset"

    external add_rule
      :  Unix.file_descr
      -> Unix.file_descr
      -> int64
      -> unit
      = "dune_spawn_landlock_add_rule"

    external restrict_self : Unix.file_descr -> unit = "dune_spawn_landlock_restrict_self"
  end

  type t =
    { handled_access : int64
    ; writable_directories : Fd.t list
    }

  let minimum_abi = 3

  let available () =
    match Raw.abi_version () with
    | abi -> abi >= minimum_abi && not (Int64.equal (Raw.write_access_rights abi) 0L)
    | exception Unix.Unix_error _ -> false
  ;;

  let write_access_rights () =
    let abi = Raw.abi_version () in
    if abi < minimum_abi then Code_error.raise "Landlock is not available" [];
    let write_access = Raw.write_access_rights abi in
    if Int64.equal write_access 0L
    then Code_error.raise "Landlock write restrictions are not available" [];
    write_access
  ;;

  let add_writable_directories directories t =
    { t with writable_directories = t.writable_directories @ directories }
  ;;

  let allow_writes_to_directories writable_directories =
    { handled_access = write_access_rights (); writable_directories }
  ;;

  let with_ruleset { handled_access; writable_directories } ~f =
    let fd = Raw.create_ruleset handled_access |> Fd.unsafe_of_unix_file_descr in
    Exn.protect
      ~finally:(fun () -> Fd.close fd)
      ~f:(fun () ->
        let raw_fd = Fd.unsafe_to_unix_file_descr fd in
        List.iter writable_directories ~f:(fun directory ->
          Raw.add_rule raw_fd (Fd.unsafe_to_unix_file_descr directory) handled_access);
        f fd)
  ;;

  let restrict t =
    with_ruleset t ~f:(fun fd -> Raw.restrict_self (Fd.unsafe_to_unix_file_descr fd))
  ;;
end

module Pgid = struct
  type t =
    | New
    | Pid of Pid.t

  let new_process_group = New
  let of_pid p = Pid p

  let to_int = function
    | New -> 0
    | Pid p -> Pid.to_int p
  ;;
end

external spawn_unix_raw
  :  env:(Env.Var.t * string) list option
  -> cwd:Working_dir.raw
  -> prog:string
  -> argv0:string
  -> args:string Array.Immutable.t
  -> stdin:Unix.file_descr
  -> stdout:Unix.file_descr
  -> stderr:Unix.file_descr
  -> use_vfork:bool
  -> setpgid:int option
  -> sigprocmask:(Unix.sigprocmask_command * int list) option
  -> landlock_fd:Unix.file_descr option
  -> pdeathsig:int
  -> int
  = "dune_spawn_unix_byte" "dune_spawn_unix"

let spawn_unix
      ~env
      ~cwd
      ~prog
      ~argv0
      ~args
      ~stdin
      ~stdout
      ~stderr
      ~use_vfork
      ~setpgid
      ~sigprocmask
      ~landlock
      ~pdeathsig
  =
  let env = Option.map env ~f:Env.to_list in
  let setpgid = Option.map ~f:Pgid.to_int setpgid in
  let pdeathsig = Signal.to_int pdeathsig in
  let sigprocmask =
    Option.map sigprocmask ~f:(fun (mask, signals) ->
      mask, List.map signals ~f:Signal.to_int)
  in
  let spawn landlock_fd =
    spawn_unix_raw
      ~env
      ~cwd:(Working_dir.raw cwd)
      ~prog
      ~argv0
      ~args
      ~stdin:(Fd.unsafe_to_unix_file_descr stdin)
      ~stdout:(Fd.unsafe_to_unix_file_descr stdout)
      ~stderr:(Fd.unsafe_to_unix_file_descr stderr)
      ~use_vfork
      ~setpgid
      ~sigprocmask
      ~landlock_fd:(Option.map landlock_fd ~f:Fd.unsafe_to_unix_file_descr)
      ~pdeathsig
    |> Pid.of_int_exn
  in
  (* CR-soon rgrinberg: [landlock] is a policy recipe, so this creates an
     equivalent kernel ruleset for every process. Create one ruleset per action
     and reuse its descriptor for every process executing that action. *)
  match landlock with
  | None -> spawn None
  | Some landlock -> Landlock.with_ruleset landlock ~f:(fun fd -> spawn (Some fd))
;;

external spawn_windows_raw
  :  env:string option
  -> cwd:string option
  -> prog:string
  -> cmdline:string
  -> stdin:Unix.file_descr
  -> stdout:Unix.file_descr
  -> stderr:Unix.file_descr
  -> int
  = "dune_spawn_windows_byte" "dune_spawn_windows"

let maybe_quote f =
  if String.contains f ' ' || String.contains f '\"' || String.contains f '\t' || f = ""
  then Filename.quote f
  else f
;;

let spawn_windows
      ~env
      ~cwd
      ~prog
      ~argv0
      ~args
      ~stdin
      ~stdout
      ~stderr
      ~use_vfork:_
      ~setpgid:_
      ~sigprocmask:_
      ~pdeathsig:_
  =
  let env = Option.map env ~f:Env.to_windows_block in
  let cwd =
    match (cwd : Working_dir.t) with
    | Path p -> Some p
    | Fd _ -> invalid_arg "Spawn.spawn: [cwd=Fd _] is not supported on Windows"
    | Inherit -> None
  in
  let argv = maybe_quote argv0 :: Array.Immutable.to_list_map args ~f:maybe_quote in
  let cmdline = String.concat argv ~sep:" " in
  let prog =
    match Filename.is_relative prog, cwd with
    | true, Some p -> Filename.concat p prog
    | _ -> prog
  in
  spawn_windows_raw
    ~env
    ~cwd
    ~prog
    ~cmdline
    ~stdin:(Fd.unsafe_to_unix_file_descr stdin)
    ~stdout:(Fd.unsafe_to_unix_file_descr stdout)
    ~stderr:(Fd.unsafe_to_unix_file_descr stderr)
  |> Pid.of_int_exn
;;

let no_null s =
  if String.contains s '\000'
  then
    Printf.ksprintf
      invalid_arg
      "Spawn.spawn: NUL bytes are not allowed in any of the arguments but found one in %S"
      s
;;

let default_stdin = Fd.unsafe_of_unix_file_descr Unix.stdin
let default_stdout = Fd.unsafe_of_unix_file_descr Unix.stdout
let default_stderr = Fd.unsafe_of_unix_file_descr Unix.stderr
let use_vfork = Poly.equal Platform.OS.value Linux

let spawn
      ?env
      ?(cwd = Working_dir.inherit_)
      ~prog
      ~argv0
      ~args
      ?(stdin = default_stdin)
      ?(stdout = default_stdout)
      ?(stderr = default_stderr)
      ?setpgid
      ?(pdeathsig = Signal.Kill)
      ?sigprocmask
      ?landlock
      ()
  =
  (match cwd with
   | Path s -> no_null s
   | Fd _ | Inherit -> ());
  no_null prog;
  no_null argv0;
  Array.Immutable.iter args ~f:no_null;
  if Sys.win32
  then
    spawn_windows
      ~env
      ~cwd
      ~prog
      ~argv0
      ~args
      ~stdin
      ~stdout
      ~stderr
      ~use_vfork
      ~setpgid
      ~sigprocmask
      ~pdeathsig
  else
    spawn_unix
      ~env
      ~cwd
      ~prog
      ~argv0
      ~args
      ~stdin
      ~stdout
      ~stderr
      ~use_vfork
      ~setpgid
      ~sigprocmask
      ~landlock
      ~pdeathsig
;;

external safe_pipe_raw : unit -> Unix.file_descr * Unix.file_descr = "dune_spawn_pipe"

let safe_pipe () =
  let read, write = if Sys.win32 then Unix.pipe ~cloexec:true () else safe_pipe_raw () in
  Fd.unsafe_of_unix_file_descr read, Fd.unsafe_of_unix_file_descr write
;;

open StdLabels

external is_osx : unit -> bool = "spawn_is_osx" [@@noalloc]

let is_osx = is_osx ()

module Working_dir = struct
  type t =
    | Path of string
    | Fd of Unix.file_descr
    | Inherit
end

module Unix_backend = struct
  type t =
    | Fork
    | Vfork

  let default =
    match Sys.getenv "SPAWN_USE_FORK" with
    | _ -> Fork
    | exception Not_found ->
      (* We observed issues in the past when using [vfork] on OSX. More
         precisely, it seems that [chdir]/[fchdir] is not taken into account
         after a vfork. We tried working around this by not doing the directory
         change in the sub-process when using [vfork] on OSX, and instead doing
         it in the parent via [pthread_chdir]/[pthread_fchdir]. This was
         unsuccessful.

         In the end we decided not to default to [vfork] on OSX. *)
      if is_osx then Fork else Vfork
  ;;
end

let no_null s =
  if String.contains s '\000'
  then
    Printf.ksprintf
      invalid_arg
      "Spawn.Env.of_list: NUL bytes are not allowed in the environment but found one in \
       %S"
      s
;;

module type Env = sig
  type t

  val of_list : string list -> t
end

module Env_win32 : Env = struct
  type t = string

  let of_list env =
    if env = []
    then "\000\000"
    else (
      let len = List.fold_left env ~init:1 ~f:(fun acc s -> acc + String.length s + 1) in
      let buf = Buffer.create len in
      List.iter env ~f:(fun s ->
        no_null s;
        Buffer.add_string buf s;
        Buffer.add_char buf '\000');
      Buffer.add_char buf '\000';
      Buffer.contents buf)
  ;;
end

module Env_unix : Env = struct
  type t = string list

  let of_list l =
    List.iter l ~f:no_null;
    l
  ;;
end

module Env : Env = (val if Sys.win32 then (module Env_win32) else (module Env_unix) : Env)

module Pgid = struct
  type t = int

  let new_process_group = 0

  let of_pid = function
    | 0 -> raise (Invalid_argument "bad pid: 0 (hint: use [Pgid.new_process_group])")
    | t -> if t < 0 then raise (Invalid_argument ("bad pid: " ^ string_of_int t)) else t
  ;;
end

external spawn_unix
  :  env:Env.t option
  -> cwd:Working_dir.t
  -> prog:string
  -> argv:string list
  -> stdin:Unix.file_descr
  -> stdout:Unix.file_descr
  -> stderr:Unix.file_descr
  -> use_vfork:bool
  -> setpgid:int option
  -> sigprocmask:(Unix.sigprocmask_command * int list) option
  -> int
  = "spawn_unix_byte" "spawn_unix"

external spawn_windows
  :  env:Env.t option
  -> cwd:string option
  -> prog:string
  -> cmdline:string
  -> stdin:Unix.file_descr
  -> stdout:Unix.file_descr
  -> stderr:Unix.file_descr
  -> int
  = "spawn_windows_byte" "spawn_windows"

let maybe_quote f =
  if String.contains f ' ' || String.contains f '\"' || String.contains f '\t' || f = ""
  then Filename.quote f
  else f
;;

let spawn_windows
  ~env
  ~cwd
  ~prog
  ~argv
  ~stdin
  ~stdout
  ~stderr
  ~use_vfork:_
  ~setpgid:_
  ~sigprocmask:_
  =
  let cwd =
    match (cwd : Working_dir.t) with
    | Path p -> Some p
    | Fd _ -> invalid_arg "Spawn.spawn: [cwd=Fd _] is not supported on Windows"
    | Inherit -> None
  in
  let cmdline = String.concat (List.map argv ~f:maybe_quote) ~sep:" " in
  let prog =
    match Filename.is_relative prog, cwd with
    | true, Some p -> Filename.concat p prog
    | _ -> prog
  in
  spawn_windows ~env ~cwd ~prog ~cmdline ~stdin ~stdout ~stderr
;;

let no_null s =
  if String.contains s '\000'
  then
    Printf.ksprintf
      invalid_arg
      "Spawn.spawn: NUL bytes are not allowed in any of the arguments but found one in %S"
      s
;;

let spawn
  ?env
  ?(cwd = Working_dir.Inherit)
  ~prog
  ~argv
  ?(stdin = Unix.stdin)
  ?(stdout = Unix.stdout)
  ?(stderr = Unix.stderr)
  ?(unix_backend = Unix_backend.default)
  ?setpgid
  ?sigprocmask
  ()
  =
  (match cwd with
   | Path s -> no_null s
   | Fd _ | Inherit -> ());
  no_null prog;
  List.iter argv ~f:no_null;
  let backend = if Sys.win32 then spawn_windows else spawn_unix in
  let use_vfork =
    match unix_backend with
    | Vfork -> true
    | Fork -> false
  in
  backend ~env ~cwd ~prog ~argv ~stdin ~stdout ~stderr ~use_vfork ~setpgid ~sigprocmask
;;

external safe_pipe : unit -> Unix.file_descr * Unix.file_descr = "spawn_pipe"

let safe_pipe = if Sys.win32 then fun () -> Unix.pipe ~cloexec:true () else safe_pipe

open! Stdune

type t =
  { pid : Pid.t
  ; wait_for_watches_established : unit -> unit
  }

let pid t = t.pid

let buffer_capacity = 65536

(* Fixed-size buffer for reading line-by-line from file descriptors. Bug:
   deadlocks if there's a line longer than the capacity of the buffer. TODO: use
   Out_channel? *)
module Buffer = struct
  type buffer =
    { data : Bytes.t
    ; mutable size : int
    }

  let create ~capacity = { data = Bytes.create capacity; size = 0 }

  let read_lines buffer fd =
    let len =
      Unix.read fd buffer.data buffer.size (buffer_capacity - buffer.size)
    in
    buffer.size <- buffer.size + len;
    if len = 0 then
      `End_of_file
        (Bytes.sub_string buffer.data ~pos:0 ~len:(Bytes.length buffer.data))
    else
      `Ok
        (let lines = ref [] in
         let line_start = ref 0 in
         for i = 0 to buffer.size - 1 do
           let c = Bytes.get buffer.data i in
           if c = '\n' || c = '\r' then (
             (if !line_start < i then
               let line =
                 Bytes.sub_string buffer.data ~pos:!line_start
                   ~len:(i - !line_start)
               in
               lines := line :: !lines);
             line_start := i + 1
           )
         done;
         buffer.size <- buffer.size - !line_start;
         Bytes.blit ~src:buffer.data ~src_pos:!line_start ~dst:buffer.data
           ~dst_pos:0 ~len:buffer.size;
         List.rev !lines)
end

module Inotify = struct
  let wait_for_watches_established stderr =
    let buffer = Buffer.create ~capacity:65536 in
    let rec loop () =
      match Buffer.read_lines buffer stderr with
      | `End_of_file _last_line -> `Error
      | `Ok lines ->
        if List.exists lines ~f:(String.equal "Watches established.") then
          `Established
        else
          loop ()
    in
    loop ()

  let parse_message s =
    match String.drop_prefix ~prefix:"e:" s with
    | None -> Error "invalid message (prefix missing)"
    | Some event -> (
      match String.lsplit2 ~on:':' event with
      | Some (_kind, path) -> Ok path
      | None -> Error "invalid message (event type missing)")
end

let command ~root =
  let excludes =
    [ {|/_build|}
    ; {|/_opam|}
    ; {|/_esy|}
    ; {|/\..+|}
    ; {|~$|}
    ; {|/#[^#]*#$|}
    ; {|4913|} (* https://github.com/neovim/neovim/issues/3460 *)
    ]
  in
  let path = Path.to_string_maybe_quoted root in
  match
    if Sys.linux then
      Bin.which ~path:(Env.path Env.initial) "inotifywait"
    else
      None
  with
  | Some inotifywait ->
    (* On Linux, use inotifywait. *)
    let excludes = String.concat ~sep:"|" excludes in
    ( inotifywait
    , [ "-r"
      ; path
      ; "--exclude"
      ; excludes
      ; "-e"
      ; "close_write"
      ; "-e"
      ; "delete"
      ; "--format"
      ; "e:%e:%w%f"
      ; "-m"
      ]
    , Inotify.parse_message
    , Some Inotify.wait_for_watches_established )
  | None -> (
    (* On all other platforms, try to use fswatch. fswatch's event filtering is
       not reliable (at least on Linux), so don't try to use it, instead act on
       all events. *)
    match Bin.which ~path:(Env.path Env.initial) "fswatch" with
    | Some fswatch ->
      let excludes =
        List.concat_map excludes ~f:(fun x -> [ "--exclude"; x ])
      in
      ( fswatch
      , [ "-r"
        ; path
        ; "--event"
        ; "Created"
        ; "--event"
        ; "Updated"
        ; "--event"
        ; "Removed"
        ]
        @ excludes
      , (fun s -> Ok s)
      , None )
    | None ->
      User_error.raise
        [ Pp.text
            (if Sys.linux then
              "Please install inotifywait to enable watch mode. If inotifywait \
               is unavailable, fswatch may also be used but will result in a \
               worse experience."
            else
              "Please install fswatch to enable watch mode.")
        ])

let spawn_external_watcher ~root =
  let prog, args, parse_line, wait_for_start = command ~root in
  let prog = Path.to_absolute_filename prog in
  let argv = prog :: args in
  let r_stdout, w_stdout = Unix.pipe () in
  let stderr, wait =
    match wait_for_start with
    | None -> (None, fun () -> ())
    | Some wait -> (
      let r_stderr, w_stderr = Unix.pipe () in
      ( Some w_stderr
      , fun () ->
          match wait r_stderr with
          | `Error -> failwith "error waiting for watches to be established"
          | `Established -> () ))
  in
  let pid = Spawn.spawn () ~prog ~argv ~stdout:w_stdout ?stderr |> Pid.of_int in
  Unix.close w_stdout;
  Option.iter stderr ~f:Unix.close;
  ((r_stdout, parse_line, wait), pid)

let create_no_buffering ~thread_safe_send_files_changed ~root =
  let (pipe, parse_line, wait), pid = spawn_external_watcher ~root in
  let worker_thread pipe =
    let buffer = Buffer.create ~capacity:buffer_capacity in
    while true do
      let lines =
        List.map
          (match Buffer.read_lines buffer pipe with
          | `End_of_file _ -> failwith "end of file reading inotify pipe"
          | `Ok lines -> lines)
          ~f:(fun line ->
            match parse_line line with
            | Error s -> failwith s
            | Ok path -> Path.of_string path)
      in
      thread_safe_send_files_changed lines
    done
  in
  ignore (Thread.create worker_thread pipe : Thread.t);
  { pid; wait_for_watches_established = wait }

let with_buffering ~create ~thread_safe_send_files_changed ~debounce_interval =
  let files_changed = ref [] in
  let event_mtx = Mutex.create () in
  let event_cv = Condition.create () in
  let res =
    create ~thread_safe_send_files_changed:(fun lines ->
        Mutex.lock event_mtx;
        files_changed := List.rev_append lines !files_changed;
        Condition.signal event_cv;
        Mutex.unlock event_mtx)
  in
  (* The buffer thread is used to avoid flooding the main thread with file
     changes events when a lot of file changes are reported at once. In
     particular, this avoids restarting the build over and over in a short
     period of time when many events are reported at once.

     It works as follow:

     - when the first event is received, send it to the main thread immediately
     so that we get a fast response time

     - after the first event is received, buffer subsequent events for
     [debounce_interval] *)
  let rec buffer_thread () =
    Mutex.lock event_mtx;
    while List.is_empty !files_changed do
      Condition.wait event_cv event_mtx
    done;
    let files = !files_changed in
    files_changed := [];
    Mutex.unlock event_mtx;
    thread_safe_send_files_changed files;
    Thread.delay debounce_interval;
    buffer_thread ()
  in
  ignore (Thread.create buffer_thread () : Thread.t);
  res

let create ~root ~debounce_interval ~thread_safe_send_files_changed =
  match debounce_interval with
  | None -> create_no_buffering ~root ~thread_safe_send_files_changed
  | Some debounce_interval ->
    with_buffering ~thread_safe_send_files_changed ~debounce_interval
      ~create:(create_no_buffering ~root)

let create_default =
  create ~root:Path.root ~debounce_interval:(Some 0.5 (* seconds *))

let wait_watches_established_blocking t = t.wait_for_watches_established ()

module For_tests = struct
  let suspend t = Unix.kill (Pid.to_int t.pid) Sys.sigstop

  let resume t = Unix.kill (Pid.to_int t.pid) Sys.sigcont
end

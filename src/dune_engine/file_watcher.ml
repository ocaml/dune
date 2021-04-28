open! Stdune
open Import

type t = Pid.t

let pid t = t

let command =
  lazy
    (let excludes =
       [ {|/_build|}
       ; {|/_opam|}
       ; {|/_esy|}
       ; {|/\..+|}
       ; {|~$|}
       ; {|/#[^#]*#$|}
       ; {|4913|} (* https://github.com/neovim/neovim/issues/3460 *)
       ]
     in
     let path = Path.to_string_maybe_quoted Path.root in
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
         ; "%w%f"
         ; "-m"
         ; "-q"
         ] )
     | None -> (
       (* On all other platforms, try to use fswatch. fswatch's event filtering
          is not reliable (at least on Linux), so don't try to use it, instead
          act on all events. *)
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
           @ excludes )
       | None ->
         User_error.raise
           [ Pp.text
               (if Sys.linux then
                 "Please install inotifywait to enable watch mode. If \
                  inotifywait is unavailable, fswatch may also be used but \
                  will result in a worse experience."
               else
                 "Please install fswatch to enable watch mode.")
           ]))

let buffering_time = 0.5 (* seconds *)

let buffer_capacity = 65536

type buffer =
  { data : Bytes.t
  ; mutable size : int
  }

let read_lines buffer fd =
  let len =
    Unix.read fd buffer.data buffer.size (buffer_capacity - buffer.size)
  in
  buffer.size <- buffer.size + len;
  let lines = ref [] in
  let line_start = ref 0 in
  for i = 0 to buffer.size - 1 do
    let c = Bytes.get buffer.data i in
    if c = '\n' || c = '\r' then (
      (if !line_start < i then
        let line =
          Bytes.sub_string buffer.data ~pos:!line_start ~len:(i - !line_start)
        in
        lines := line :: !lines);
      line_start := i + 1
    )
  done;
  buffer.size <- buffer.size - !line_start;
  Bytes.blit ~src:buffer.data ~src_pos:!line_start ~dst:buffer.data ~dst_pos:0
    ~len:buffer.size;
  List.rev !lines

let spawn_external_watcher () =
  let prog, args = Lazy.force command in
  let prog = Path.to_absolute_filename prog in
  let argv = prog :: args in
  let r, w = Unix.pipe () in
  let pid = Spawn.spawn () ~prog ~argv ~stdout:w |> Pid.of_int in
  Unix.close w;
  (r, pid)

let create ~thread_safe_send_files_changed =
  let files_changed = ref [] in
  let event_mtx = Mutex.create () in
  let event_cv = Condition.create () in
  let worker_thread pipe =
    let buffer = { data = Bytes.create buffer_capacity; size = 0 } in
    while true do
      let lines = List.map (read_lines buffer pipe) ~f:Path.of_string in
      Mutex.lock event_mtx;
      files_changed := List.rev_append lines !files_changed;
      Condition.signal event_cv;
      Mutex.unlock event_mtx
    done
  in
  (* The buffer thread is used to avoid flooding the main thread with file
     changes events when a lot of file changes are reported at once. In
     particular, this avoids restarting the build over and over in a short
     period of time when many events are reported at once.

     It works as follow:

     - when the first event is received, send it to the main thread immediately
     so that we get a fast response time

     - after the first event is received, buffer subsequent events for
     [buffering_time] *)
  let rec buffer_thread () =
    Mutex.lock event_mtx;
    while List.is_empty !files_changed do
      Condition.wait event_cv event_mtx
    done;
    let files = !files_changed in
    files_changed := [];
    Mutex.unlock event_mtx;
    thread_safe_send_files_changed files;
    Thread.delay buffering_time;
    buffer_thread ()
  in
  let pipe, pid = spawn_external_watcher () in
  ignore (Thread.create worker_thread pipe : Thread.t);
  ignore (Thread.create buffer_thread () : Thread.t);
  pid

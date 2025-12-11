open Stdune
module Timestamp = Chrome_trace.Event.Timestamp

module Mac = struct
  external open_fds : pid:int -> int = "dune_trace_open_fds"
  external available : unit -> bool = "dune_trace_available"
end

type dst =
  | Out of out_channel
  | Custom of
      { write : string -> unit
      ; close : unit -> unit
      ; flush : unit -> unit
      }

type t =
  { print : string -> unit
  ; close : unit -> unit
  ; flush : unit -> unit
  ; mutable after_first_event : bool
  }

(* all fields of record used *)

let close { print; close; _ } =
  print "]\n";
  close ()
;;

let global = ref None

let () =
  at_exit (fun () ->
    match !global with
    | None -> ()
    | Some t -> close t)
;;

let set_global t =
  if Option.is_some !global then Code_error.raise "global stats have been set" [];
  global := Some t
;;

let global () = !global

let create dst =
  let print =
    match dst with
    | Out out -> Stdlib.output_string out
    | Custom c -> c.write
  in
  let close =
    match dst with
    | Out out -> fun () -> Stdlib.close_out out
    | Custom c -> c.close
  in
  let flush =
    match dst with
    | Out out -> fun () -> flush out
    | Custom c -> c.flush
  in
  { print; close; after_first_event = false; flush }
;;

let flush t = t.flush ()

let next_leading_char t =
  match t.after_first_event with
  | true -> ','
  | false ->
    t.after_first_event <- true;
    '['
;;

let printf t format_string =
  let c = next_leading_char t in
  Printf.ksprintf t.print ("%c" ^^ format_string ^^ "\n") c
;;

module Event = struct
  module Async = struct
    type data =
      { args : Chrome_trace.Event.args option
      ; cat : string list option
      ; name : string
      }

    type nonrec t =
      { t : t
      ; event_data : data
      ; start : float
      }

    let create_sandbox ~loc =
      { args = Some [ "loc", `String (Loc.to_file_colon_line loc) ]
      ; name = "create-sandbox"
      ; cat = Some [ "sandbox" ]
      }
    ;;

    let fetch ~url ~target ~checksum =
      let args = [ "url", `String url; "target", `String (Path.to_string target) ] in
      let args =
        match checksum with
        | None -> args
        | Some c -> ("checksum", `String c) :: args
      in
      { args = Some args; cat = Some [ "pkg" ]; name = "fetch" }
    ;;
  end

  type t = Chrome_trace.Event.t

  let scan_source ~name ~start ~stop ~dir =
    let module Event = Chrome_trace.Event in
    let module Timestamp = Event.Timestamp in
    let dur = Timestamp.of_float_seconds (stop -. start) in
    let common =
      Event.common_fields
        ~name:(name ^ ": " ^ Path.Source.to_string dir)
        ~ts:(Timestamp.of_float_seconds start)
        ()
    in
    let args = [ "dir", `String (Path.Source.to_string dir) ] in
    Event.complete common ~args ~dur
  ;;

  let evalauted_rules ~rule_total =
    let open Chrome_trace in
    let args = [ "value", `Int rule_total ] in
    let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
    let common = Event.common_fields ~name:"evaluated_rules" ~ts () in
    Event.counter common args
  ;;

  let config ~version =
    let args =
      let args =
        [ "build_dir", `String (Path.Build.to_string Path.Build.root)
        ; "argv", `List (Array.to_list Sys.argv |> List.map ~f:Json.string)
        ; "env", `List (Unix.environment () |> Array.to_list |> List.map ~f:Json.string)
        ; "root", `String Path.(to_absolute_filename root)
        ]
      in
      match version with
      | None -> args
      | Some v -> ("version", Stdune.Json.string v) :: args
    in
    let open Chrome_trace in
    let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
    let common = Event.common_fields ~cat:[ "config" ] ~name:"config" ~ts () in
    Event.instant ~args common
  ;;

  let scheduler_idle () =
    let fields =
      let ts = Chrome_trace.Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
      Chrome_trace.Event.common_fields ~name:"watch mode iteration" ~ts ()
    in
    (* the instant event allows us to separate build commands from
       different iterations of the watch mode in the event viewer *)
    Chrome_trace.Event.instant ~scope:Global fields
  ;;

  module Exit_status = struct
    type error =
      | Failed of int
      | Signaled of Signal.t

    type t = (int, error) result
  end

  type targets =
    { root : Path.Build.t
    ; files : Filename.Set.t
    ; dirs : Filename.Set.t
    }

  let args_of_targets =
    let paths root name set =
      if Filename.Set.is_empty set
      then []
      else
        [ ( name
          , `List
              (Filename.Set.to_list_map set ~f:(fun x ->
                 `String (Path.Build.relative root x |> Path.Build.to_string))) )
        ]
    in
    fun { root; files; dirs } ->
      paths root "target_files" files @ paths root "target_dirs" dirs
  ;;

  let process
        ~name
        ~started_at
        ~targets
        ~categories
        ~pid
        ~exit
        ~prog
        ~process_args
        ~dir
        ~stdout
        ~stderr
        ~(times : Proc.Times.t)
    =
    let open Chrome_trace in
    let common =
      let name =
        match name with
        | Some n -> n
        | None -> Filename.basename prog
      in
      let ts = Timestamp.of_float_seconds started_at in
      Event.common_fields ~cat:("process" :: categories) ~name ~ts ()
    in
    let always =
      [ "process_args", `List (List.map process_args ~f:(fun arg -> `String arg))
      ; "pid", `Int (Pid.to_int pid)
      ]
    in
    let extended =
      let exit =
        match exit with
        | Ok n -> [ "exit", `Int n ]
        | Error (Exit_status.Failed n) ->
          [ "exit", `Int n; "error", `String (sprintf "exited with code %d" n) ]
        | Error (Signaled s) ->
          [ "exit", `Int (Signal.to_int s)
          ; "error", `String (sprintf "got signal %s" (Signal.name s))
          ]
      in
      let output name s =
        match s with
        | "" -> []
        | s -> [ name, `String s ]
      in
      List.concat
        [ [ "prog", `String prog
          ; "dir", `String (Option.map ~f:Path.to_string dir |> Option.value ~default:".")
          ]
        ; exit
        ; (match targets with
           | None -> []
           | Some targets -> args_of_targets targets)
        ; output "stdout" stdout
        ; output "stderr" stderr
        ]
    in
    let args = always @ extended in
    let dur = Event.Timestamp.of_float_seconds times.elapsed_time in
    Event.complete ~args ~dur common
  ;;

  let persistent ~file ~module_ what ~start ~stop =
    let module Event = Chrome_trace.Event in
    let module Timestamp = Event.Timestamp in
    let dur = Timestamp.of_float_seconds (stop -. start) in
    let common =
      Event.common_fields ~name:"db" ~ts:(Timestamp.of_float_seconds start) ()
    in
    let args =
      [ "path", `String (Path.to_string file)
      ; "module", `String module_
      ; ( "operation"
        , `String
            (match what with
             | `Save -> "save"
             | `Load -> "load") )
      ]
    in
    Event.complete common ~args ~dur
  ;;

  module Rpc = struct
    type stage =
      | Start
      | Stop

    let async_kind_of_stage = function
      | Start -> Chrome_trace.Event.Start
      | Stop -> End
    ;;

    let session ~id stage =
      let open Chrome_trace in
      let common =
        let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
        Event.common_fields ~ts ~name:"rpc_session" ()
      in
      let id = Chrome_trace.Id.create (`Int id) in
      Event.async id (async_kind_of_stage stage) common
    ;;

    let rec to_json : Sexp.t -> Chrome_trace.Json.t = function
      | Atom s -> `String s
      | List s -> `List (List.map s ~f:to_json)
    ;;

    let message what ~meth_ ~id stage =
      let open Chrome_trace in
      let name =
        match what with
        | `Notification -> "notification"
        | `Request _ -> "request"
      in
      let args = [ "meth", `String meth_ ] in
      let args =
        match what with
        | `Notification -> args
        | `Request id -> ("request_id", to_json id) :: args
      in
      let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
      let common = Event.common_fields ~cat:[ "rpc" ] ~ts ~name () in
      Event.async
        (Chrome_trace.Id.create (`Int id))
        ~args
        (async_kind_of_stage stage)
        common
    ;;
  end
end

let emit t event = printf t "%s" (Json.to_string (Chrome_trace.Event.to_json event))

let start t k : Event.Async.t option =
  match t with
  | None -> None
  | Some t ->
    let event_data = k () in
    let start = Unix.gettimeofday () in
    Some { t; event_data; start }
;;

let finish event =
  match event with
  | None -> ()
  | Some { Event.Async.t; start; event_data = { args; cat; name } } ->
    let dur =
      let stop = Unix.gettimeofday () in
      Timestamp.of_float_seconds (stop -. start)
    in
    let common =
      Chrome_trace.Event.common_fields
        ?cat
        ~name
        ~ts:(Timestamp.of_float_seconds start)
        ()
    in
    let event = Chrome_trace.Event.complete ?args common ~dur in
    emit t event
;;

module Fd_count = struct
  type t =
    | Unknown
    | This of int

  let lsof =
    let prog = lazy (Bin.which ~path:(Env_path.path Env.initial) "lsof") in
    (* note: we do not use the Process module here, because it would create a
       circular dependency *)
    fun () ->
      match Lazy.force prog with
      | None -> Unknown
      | Some prog ->
        let lsof_r, lsof_w = Unix.pipe ~cloexec:true () in
        let prog = Path.to_string prog in
        let pid =
          let argv =
            [ prog; "-l"; "-O"; "-P"; "-n"; "-w"; "-p"; string_of_int (Unix.getpid ()) ]
          in
          Spawn.spawn ~prog ~argv ~stdout:lsof_w () |> Pid.of_int
        in
        Unix.close lsof_w;
        (match
           let _, status = Unix.waitpid [] (Pid.to_int pid) in
           status
         with
         | Unix.WEXITED 0 ->
           let count =
             let chan = Unix.in_channel_of_descr lsof_r in
             let rec loop acc =
               match input_line chan with
               | exception End_of_file -> acc
               | _ -> loop (acc + 1)
             in
             (* the output contains a header line *)
             let res = loop (-1) in
             Io.close_in chan;
             res
           in
           This count
         | (exception Unix.Unix_error (_, _, _))
         (* The final [waitpid] call fails with:

            {[
              Error: waitpid(): No child processes
            ]} *)
         | _ ->
           Unix.close lsof_r;
           Unknown)
  ;;

  let proc_fs () =
    match Sys.readdir "/proc/self/fd" with
    | files -> This (Array.length files - 1 (* -1 for the dirfd *))
    | exception Sys_error _ -> Unknown
  ;;

  let how = ref `Unknown
  let pid = lazy (Unix.getpid ())

  let get () =
    match !how with
    | `Disable -> Unknown
    | `Lsof -> lsof ()
    | `Proc_fs -> proc_fs ()
    | `Mac -> This (Mac.open_fds ~pid:(Lazy.force pid))
    | `Unknown ->
      if Mac.available ()
      then (
        how := `Mac;
        This (Mac.open_fds ~pid:(Lazy.force pid)))
      else (
        match proc_fs () with
        | This _ as n ->
          how := `Proc_fs;
          n
        | Unknown ->
          let res = lsof () in
          (how
           := match res with
              | This _ -> `Lsof
              | Unknown -> `Disable);
          res)
  ;;
end

let record_gc_and_fd stats =
  let module Event = Chrome_trace.Event in
  let module Json = Chrome_trace.Json in
  let ts = Event.Timestamp.of_float_seconds (Unix.gettimeofday ()) in
  let () =
    let common = Event.common_fields ~name:"gc" ~ts () in
    let args =
      let stat = Gc.quick_stat () in
      [ "stack_size", `Int stat.stack_size
      ; "heap_words", `Int stat.heap_words
      ; "top_heap_words", `Int stat.top_heap_words
      ; "minor_words", `Float stat.minor_words
      ; "major_words", `Float stat.major_words
      ; "promoted_words", `Float stat.promoted_words
      ; "compactions", `Int stat.compactions
      ; "major_collections", `Int stat.major_collections
      ; "minor_collections", `Int stat.minor_collections
      ]
    in
    let event = Event.counter common args in
    emit stats event
  in
  match Fd_count.get () with
  | Unknown -> ()
  | This fds ->
    let event =
      let args = [ "value", `Int fds ] in
      let common = Event.common_fields ~name:"fds" ~ts () in
      Event.counter common args
    in
    emit stats event
;;

module Private = struct
  module Fd_count = Fd_count
end

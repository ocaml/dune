open Stdune

module Arg = struct
  include Json

  let rec sexp : Sexp.t -> t = function
    | Atom s -> string s
    | List xs -> list (List.map ~f:sexp xs)
  ;;

  let dyn dyn = sexp (Sexp.of_dyn dyn)
  let path p = string (Path.to_string p)
  let source_path p = string (Path.Source.to_string p)
  let build_path p = string (Path.Build.to_string p)
  let record x = `Assoc x
end

module Event = struct
  module Id = struct
    open Chrome_trace.Id

    let int x = create (`Int x)
    let string x = create (`String x)
  end

  module Event = Chrome_trace.Event

  let make_ts ts = Event.Timestamp.of_float_seconds (Time.to_secs ts)
  let make_dur span = Event.Timestamp.of_float_seconds (Time.Span.to_secs span)

  type args = (string * Arg.t) list
  type t = Event.t

  let complete ?args ~name ~start ~dur cat =
    Event.common_fields ~ts:(make_ts start) ~name ~cat:[ Category.to_string cat ] ()
    |> Event.complete ~dur:(make_dur dur) ?args
  ;;

  let instant ?args ~name ts cat =
    Event.common_fields ~ts:(make_ts ts) ~name ~cat:[ Category.to_string cat ] ()
    |> Event.instant ?args
  ;;

  let async ?args id ~name ts stage cat =
    Event.common_fields ~cat:[ Category.to_string cat ] ~ts:(make_ts ts) ~name ()
    |> Event.async
         ?args
         id
         (match stage with
          | `Start -> Start
          | `Stop -> End)
  ;;
end

module Async = struct
  type data =
    { args : Event.args option
    ; cat : Category.t
    ; name : string
    }

  type nonrec t =
    { event_data : data
    ; start : Time.t
    }

  let create ~event_data ~start = { event_data; start }

  let create_sandbox ~loc =
    { args = Some [ "loc", Arg.string (Loc.to_file_colon_line loc) ]
    ; name = "create-sandbox"
    ; cat = Sandbox
    }
  ;;

  let fetch ~url ~target ~checksum =
    let args =
      let args = [ "url", Arg.string url; "target", Arg.path target ] in
      match checksum with
      | None -> args
      | Some c -> ("checksum", Arg.string c) :: args
    in
    { args = Some args; cat = Pkg; name = "fetch" }
  ;;
end

type t = Event.t

let scan_source ~name ~start ~stop ~dir =
  let dur = Time.diff stop start in
  let args = [ "dir", Arg.source_path dir ] in
  Event.complete ~name ~start ~args ~dur Rules
;;

let evalauted_rules ~rule_total =
  let now = Time.now () in
  let args = [ "value", Arg.int rule_total ] in
  Event.instant ~name:"evalauted_rules" ~args now Rules
;;

let config ~version =
  let now = Time.now () in
  let args =
    let args =
      [ "build_dir", Arg.build_path Path.Build.root
      ; "argv", Arg.list (Array.to_list Sys.argv |> List.map ~f:Arg.string)
      ; "env", Arg.list (Unix.environment () |> Array.to_list |> List.map ~f:Arg.string)
      ; "root", Arg.string Path.(to_absolute_filename root)
      ; "pid", Arg.int (Unix.getpid ())
      ; "initial_cwd", Arg.string Fpath.initial_cwd
      ]
    in
    match version with
    | None -> args
    | Some v -> ("version", Arg.string v) :: args
  in
  Event.instant ~args ~name:"config" now Config
;;

let exit () =
  let now = Time.now () in
  Event.instant ~name:"exit" now Config
;;

let scheduler_idle () =
  let now = Time.now () in
  Event.instant ~name:"watch mode iteration" now Scheduler
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
        , Arg.list
            (Filename.Set.to_list_map set ~f:(fun x ->
               Arg.build_path (Path.Build.relative root x))) )
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
  let name =
    match name with
    | Some n -> n
    | None -> Filename.basename prog
  in
  let args =
    let always =
      [ "process_args", Arg.list (List.map process_args ~f:Arg.string)
      ; "pid", Arg.int (Pid.to_int pid)
      ; "categories", Arg.list (List.map categories ~f:Arg.string)
      ]
    in
    let extended =
      let exit =
        match exit with
        | Ok n -> [ "exit", Arg.int n ]
        | Error (Exit_status.Failed n) ->
          [ "exit", Arg.int n; "error", Arg.string (sprintf "exited with code %d" n) ]
        | Error (Signaled s) ->
          [ "exit", Arg.int (Signal.to_int s)
          ; "error", Arg.string (sprintf "got signal %s" (Signal.name s))
          ]
      in
      let output name s =
        match s with
        | "" -> []
        | s -> [ name, Arg.string s ]
      in
      List.concat
        [ [ "prog", Arg.string prog
          ; "dir", Arg.path (Option.value dir ~default:Path.root)
          ]
        ; exit
        ; (match targets with
           | None -> []
           | Some targets -> args_of_targets targets)
        ; output "stdout" stdout
        ; output "stderr" stderr
        ]
    in
    always @ extended
  in
  Event.complete ~args ~start:started_at ~dur:times.elapsed_time ~name Process
;;

let persistent ~file ~module_ what ~start ~stop =
  let dur = Time.diff stop start in
  let args =
    [ "path", Arg.path file
    ; "module", Arg.string module_
    ; ( "operation"
      , Arg.string
          (match what with
           | `Save -> "save"
           | `Load -> "load") )
    ]
  in
  Event.complete ~name:"db" ~args ~start ~dur Persistent
;;

module Rpc = struct
  type stage =
    [ `Start
    | `Stop
    ]

  let session ~id stage =
    let now = Time.now () in
    let id = Event.Id.int id in
    Event.async id now stage ~name:"rpc_session" Rpc
  ;;

  let rec to_json : Sexp.t -> Arg.t = function
    | Atom s -> Arg.string s
    | List s -> Arg.list (List.map s ~f:to_json)
  ;;

  let message what ~meth_ ~id stage =
    let now = Time.now () in
    let name =
      match what with
      | `Notification -> "notification"
      | `Request _ -> "request"
    in
    let args =
      let args = [ "meth", Arg.string meth_ ] in
      match what with
      | `Notification -> args
      | `Request id -> ("request_id", to_json id) :: args
    in
    Event.async (Event.Id.int id) ~args ~name now stage Rpc
  ;;

  let packet_read ~id ~success ~error =
    let now = Time.now () in
    let args =
      let base = [ "id", Arg.int id; "success", Arg.bool success ] in
      match error with
      | None -> base
      | Some err -> ("error", Arg.string err) :: base
    in
    Event.instant ~args ~name:"packet_read" now Rpc
  ;;

  let packet_write ~id ~count =
    let now = Time.now () in
    let args = [ "id", Arg.int id; "count", Arg.int count ] in
    Event.instant ~name:"packet_write" ~args now Rpc
  ;;

  let accept ~success ~error =
    let now = Time.now () in
    let args =
      let base = [ "success", Arg.bool success ] in
      match error with
      | None -> base
      | Some err -> ("error", Arg.string err) :: base
    in
    Event.instant ~args ~name:"accept" now Rpc
  ;;

  let close ~id =
    let now = Time.now () in
    let args = [ "id", Arg.int id ] in
    Event.instant ~args ~name:"close" now Rpc
  ;;
end

let gc () =
  let now = Time.now () in
  let args =
    let stat = Gc.quick_stat () in
    [ "stack_size", Arg.int stat.stack_size
    ; "heap_words", Arg.int stat.heap_words
    ; "top_heap_words", Arg.int stat.top_heap_words
    ; "minor_words", Arg.float stat.minor_words
    ; "major_words", Arg.float stat.major_words
    ; "promoted_words", Arg.float stat.promoted_words
    ; "compactions", Arg.int stat.compactions
    ; "major_collections", Arg.int stat.major_collections
    ; "minor_collections", Arg.int stat.minor_collections
    ]
  in
  Event.instant ~name:"gc" ~args now Gc
;;

let fd_count () =
  match Fd_count.get () with
  | Unknown -> None
  | This fds ->
    let now = Time.now () in
    let args = [ "value", Arg.int fds ] in
    Some (Event.instant ~name:"fds" ~args now Fd)
;;

let promote src dst =
  let now = Time.now () in
  let args = [ "src", Arg.build_path src; "dst", Arg.source_path dst ] in
  Event.instant ~name:"promote" ~args now Promote
;;

type alias =
  { dir : Path.Source.t
  ; name : string
  ; recursive : bool
  ; contexts : string list
  }

let json_of_alias { dir; name; recursive; contexts } =
  Arg.record
    [ "dir", Arg.source_path dir
    ; "name", Arg.string name
    ; "recursive", Arg.bool recursive
    ; "contexts", Arg.list (List.map contexts ~f:Arg.string)
    ]
;;

let resolve_targets targets aliases =
  let now = Time.now () in
  let args =
    [ "targets", List.map targets ~f:Arg.path
    ; "aliases", List.map aliases ~f:json_of_alias
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | [] -> None
      | _ :: _ -> Some (k, Arg.list v))
  in
  Event.instant ~args ~name:"targets" now Build
;;

let load_dir dir =
  let now = Time.now () in
  let args = [ "dir", Arg.path dir ] in
  Event.instant ~name:"load-dir" ~args now Debug
;;

let file_watcher event =
  (* CR-soon rgrinberg: this timestamp is wrong *)
  let now = Time.now () in
  let name, args =
    match event with
    | `Queue_overflow -> "queue_overflow", []
    | `Sync id -> "sync", [ "id", Arg.int id ]
    | `Watcher_terminated -> "watcher_terminated", []
    | `File (path, kind) ->
      ( (match kind with
         | `Created -> "create"
         | `Deleted -> "delete"
         | `File_changed -> "changed"
         | `Unknown -> "unknown")
      , [ "path", Arg.path path ] )
  in
  Event.instant ~name ~args now File_watcher
;;

let error loc kind exn backtrace memo_stack =
  let now = Time.now () in
  let name =
    match kind with
    | `User -> "user"
    | `Fatal -> "fatal"
  in
  let loc =
    Option.map loc ~f:(fun loc -> "loc", Arg.string (Loc.to_file_colon_line loc))
  in
  let memo_stack =
    match memo_stack with
    | [] -> None
    | frames ->
      let frames = List.map frames ~f:Arg.dyn |> Arg.list in
      Some ("memo", frames)
  in
  let backtrace =
    Option.map backtrace ~f:(fun bt ->
      "backtrace", Arg.string (Printexc.raw_backtrace_to_string bt))
  in
  let args =
    ("exn", Arg.string (Printexc.to_string exn))
    :: List.filter_opt [ loc; memo_stack; backtrace ]
  in
  Event.instant ~name ~args now Diagnostics
;;

let log { Log.Message.level; message; args } =
  let now = Time.now () in
  let name =
    match level with
    | `Warn -> "warn"
    | `Info -> "info"
    | `Verbose -> "verbose"
  in
  let args =
    ("message", `String message) :: List.map args ~f:(fun (name, v) -> name, Arg.dyn v)
  in
  Event.instant ~args ~name now Log
;;

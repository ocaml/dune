open Stdune
module Timestamp = Chrome_trace.Event.Timestamp

module Async = struct
  type data =
    { args : Chrome_trace.Event.args option
    ; cat : string list option
    ; name : string
    }

  type nonrec t =
    { event_data : data
    ; start : Time.t
    }

  let create ~event_data ~start = { event_data; start }

  let create_sandbox ~loc =
    { args = Some [ "loc", `String (Loc.to_file_colon_line loc) ]
    ; name = "create-sandbox"
    ; cat = Some [ Category.to_string Sandbox ]
    }
  ;;

  let fetch ~url ~target ~checksum =
    let args =
      let args = [ "url", `String url; "target", `String (Path.to_string target) ] in
      match checksum with
      | None -> args
      | Some c -> ("checksum", `String c) :: args
    in
    { args = Some args; cat = Some [ Category.to_string Pkg ]; name = "fetch" }
  ;;
end

type t = Chrome_trace.Event.t

let scan_source ~name ~start ~stop ~dir =
  let module Event = Chrome_trace.Event in
  let module Timestamp = Event.Timestamp in
  let dur = Time.diff stop start |> Time.Span.to_secs |> Timestamp.of_float_seconds in
  let common =
    Event.common_fields
      ~name:(name ^ ": " ^ Path.Source.to_string dir)
      ~ts:(Timestamp.of_float_seconds (Time.to_secs start))
      ()
  in
  let args = [ "dir", `String (Path.Source.to_string dir) ] in
  Event.complete common ~args ~dur
;;

let evalauted_rules ~rule_total =
  let open Chrome_trace in
  let args = [ "value", `Int rule_total ] in
  let ts = Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs) in
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
  let ts = Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs) in
  let common = Event.common_fields ~cat:[ "config" ] ~name:"config" ~ts () in
  Event.instant ~args common
;;

let scheduler_idle () =
  let fields =
    let ts =
      Chrome_trace.Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs)
    in
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
    let ts = Timestamp.of_float_seconds (Time.to_secs started_at) in
    Event.common_fields ~cat:(Category.to_string Process :: categories) ~name ~ts ()
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
  let dur = Event.Timestamp.of_float_seconds (Time.Span.to_secs times.elapsed_time) in
  Event.complete ~args ~dur common
;;

let persistent ~file ~module_ what ~start ~stop =
  let module Event = Chrome_trace.Event in
  let module Timestamp = Event.Timestamp in
  let dur = Time.diff stop start |> Time.Span.to_secs |> Timestamp.of_float_seconds in
  let common =
    Event.common_fields
      ~name:"db"
      ~ts:(Timestamp.of_float_seconds (Time.to_secs start))
      ()
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
      let ts = Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs) in
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
    let ts = Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs) in
    let common = Event.common_fields ~cat:[ Category.to_string Rpc ] ~ts ~name () in
    Event.async
      (Chrome_trace.Id.create (`Int id))
      ~args
      (async_kind_of_stage stage)
      common
  ;;

  let packet_read ~id ~success ~error =
    let open Chrome_trace in
    let ts = Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs) in
    let args =
      let base = [ "id", `Int id; "success", `Bool success ] in
      match error with
      | None -> base
      | Some err -> ("error", `String err) :: base
    in
    let common =
      Event.common_fields
        ~cat:[ Category.to_string Rpc; "packet" ]
        ~name:"packet_read"
        ~ts
        ()
    in
    Event.instant ~args common
  ;;

  let packet_write ~id ~count =
    let open Chrome_trace in
    let ts = Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs) in
    let args = [ "id", `Int id; "count", `Int count ] in
    let common =
      Event.common_fields
        ~cat:[ Category.to_string Rpc; "packet" ]
        ~name:"packet_write"
        ~ts
        ()
    in
    Event.instant ~args common
  ;;

  let accept ~success ~error =
    let open Chrome_trace in
    let ts = Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs) in
    let args =
      let base = [ "success", `Bool success ] in
      match error with
      | None -> base
      | Some err -> ("error", `String err) :: base
    in
    let common =
      Event.common_fields ~cat:[ Category.to_string Rpc; "accept" ] ~name:"accept" ~ts ()
    in
    Event.instant ~args common
  ;;

  let close ~id =
    let open Chrome_trace in
    let ts = Time.now () |> Time.to_secs |> Event.Timestamp.of_float_seconds in
    let args = [ "id", `Int id ] in
    let common =
      Event.common_fields ~cat:[ Category.to_string Rpc; "session" ] ~name:"close" ~ts ()
    in
    Event.instant ~args common
  ;;
end

let gc () =
  let module Event = Chrome_trace.Event in
  let module Json = Chrome_trace.Json in
  let ts = Time.now () |> Time.to_secs |> Event.Timestamp.of_float_seconds in
  let common = Event.common_fields ~cat:[ Category.to_string Gc ] ~name:"gc" ~ts () in
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
  Event.counter common args
;;

let fd_count () =
  let module Event = Chrome_trace.Event in
  let module Json = Chrome_trace.Json in
  let ts = Time.now () |> Time.to_secs |> Event.Timestamp.of_float_seconds in
  match Fd_count.get () with
  | Unknown -> None
  | This fds ->
    let args = [ "value", `Int fds ] in
    let common = Event.common_fields ~cat:[ Category.to_string Fd ] ~name:"fds" ~ts () in
    Some (Event.counter common args)
;;

let promote src dst =
  let module Event = Chrome_trace.Event in
  let common =
    let ts = Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs) in
    Event.common_fields ~cat:[ Category.to_string Promote ] ~name:"promote" ~ts ()
  in
  let args =
    [ "src", `String (Path.Build.to_string src)
    ; "dst", `String (Path.Source.to_string dst)
    ]
  in
  Event.instant ~args common
;;

type alias =
  { dir : Path.Source.t
  ; name : string
  ; recursive : bool
  ; contexts : string list
  }

let json_of_alias { dir; name; recursive; contexts } =
  `Assoc
    [ "dir", `String (Path.Source.to_string dir)
    ; "name", `String name
    ; "recursive", `Bool recursive
    ; "contexts", `List (List.map contexts ~f:Json.string)
    ]
;;

let resolve_targets targets aliases =
  let module Event = Chrome_trace.Event in
  let ts = Event.Timestamp.of_float_seconds (Time.now () |> Time.to_secs) in
  let args =
    [ "targets", List.map targets ~f:(fun p -> `String (Path.to_string p))
    ; "aliases", List.map aliases ~f:json_of_alias
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | [] -> None
      | _ :: _ -> Some (k, `List v))
  in
  let common =
    Event.common_fields ~cat:[ Category.to_string Build ] ~name:"targets" ~ts ()
  in
  Event.instant ~args common
;;

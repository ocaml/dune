open Stdune

module Event = struct
  module Id = Chrome_trace.Id
  module Event = Chrome_trace.Event

  let make_ts ts = Event.Timestamp.of_float_seconds (Time.to_secs ts)
  let make_dur span = Event.Timestamp.of_float_seconds (Time.Span.to_secs span)

  type args = Event.args
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
    { args = Some [ "loc", `String (Loc.to_file_colon_line loc) ]
    ; name = "create-sandbox"
    ; cat = Sandbox
    }
  ;;

  let fetch ~url ~target ~checksum =
    let args =
      let args = [ "url", `String url; "target", `String (Path.to_string target) ] in
      match checksum with
      | None -> args
      | Some c -> ("checksum", `String c) :: args
    in
    { args = Some args; cat = Pkg; name = "fetch" }
  ;;
end

type t = Event.t

let scan_source ~name ~start ~stop ~dir =
  let dur = Time.diff stop start in
  let args = [ "dir", `String (Path.Source.to_string dir) ] in
  Event.complete ~name ~start ~args ~dur Rules
;;

let evalauted_rules ~rule_total =
  let now = Time.now () in
  let args = [ "value", `Int rule_total ] in
  Event.instant ~name:"evalauted_rules" ~args now Rules
;;

let config ~version =
  let now = Time.now () in
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
  let name =
    match name with
    | Some n -> n
    | None -> Filename.basename prog
  in
  let args =
    let always =
      [ "process_args", `List (List.map process_args ~f:(fun arg -> `String arg))
      ; "pid", `Int (Pid.to_int pid)
      ; "categories", `List (List.map categories ~f:Json.string)
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
    always @ extended
  in
  Event.complete ~args ~start:started_at ~dur:times.elapsed_time ~name Process
;;

let persistent ~file ~module_ what ~start ~stop =
  let dur = Time.diff stop start in
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
  Event.complete ~name:"db" ~args ~start ~dur Persistent
;;

module Rpc = struct
  type stage =
    [ `Start
    | `Stop
    ]

  let session ~id stage =
    let now = Time.now () in
    let id = Event.Id.create (`Int id) in
    Event.async id now stage ~name:"rpc_session" Rpc
  ;;

  let rec to_json : Sexp.t -> Json.t = function
    | Atom s -> `String s
    | List s -> `List (List.map s ~f:to_json)
  ;;

  let message what ~meth_ ~id stage =
    let now = Time.now () in
    let name =
      match what with
      | `Notification -> "notification"
      | `Request _ -> "request"
    in
    let args =
      let args = [ "meth", `String meth_ ] in
      match what with
      | `Notification -> args
      | `Request id -> ("request_id", to_json id) :: args
    in
    Event.async (Event.Id.create (`Int id)) ~args ~name now stage Rpc
  ;;

  let packet_read ~id ~success ~error =
    let now = Time.now () in
    let args =
      let base = [ "id", `Int id; "success", `Bool success ] in
      match error with
      | None -> base
      | Some err -> ("error", `String err) :: base
    in
    Event.instant ~args ~name:"packet_read" now Rpc
  ;;

  let packet_write ~id ~count =
    let now = Time.now () in
    let args = [ "id", `Int id; "count", `Int count ] in
    Event.instant ~name:"packet_write" ~args now Rpc
  ;;

  let accept ~success ~error =
    let now = Time.now () in
    let args =
      let base = [ "success", `Bool success ] in
      match error with
      | None -> base
      | Some err -> ("error", `String err) :: base
    in
    Event.instant ~args ~name:"accept" now Rpc
  ;;

  let close ~id =
    let now = Time.now () in
    let args = [ "id", `Int id ] in
    Event.instant ~args ~name:"close" now Rpc
  ;;
end

let gc () =
  let now = Time.now () in
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
  Event.instant ~name:"gc" ~args now Gc
;;

let fd_count () =
  match Fd_count.get () with
  | Unknown -> None
  | This fds ->
    let now = Time.now () in
    let args = [ "value", `Int fds ] in
    Some (Event.instant ~name:"fds" ~args now Fd)
;;

let promote src dst =
  let now = Time.now () in
  let args =
    [ "src", `String (Path.Build.to_string src)
    ; "dst", `String (Path.Source.to_string dst)
    ]
  in
  Event.instant ~name:"promote" ~args now Promote
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
  let now = Time.now () in
  let args =
    [ "targets", List.map targets ~f:(fun p -> `String (Path.to_string p))
    ; "aliases", List.map aliases ~f:json_of_alias
    ]
    |> List.filter_map ~f:(fun (k, v) ->
      match v with
      | [] -> None
      | _ :: _ -> Some (k, `List v))
  in
  Event.instant ~args ~name:"targets" now Build
;;

let load_dir dir =
  let now = Time.now () in
  let args = [ "dir", `String (Path.to_string dir) ] in
  Event.instant ~name:"load-dir" ~args now Debug
;;

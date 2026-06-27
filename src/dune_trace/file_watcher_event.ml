open Stdune

type kind =
  | Created
  | Deleted
  | File_changed
  | Unknown

type t =
  [ `File of Path.t * kind
  | `Queue_overflow
  | `Sync of int
  | `Watcher_terminated
  ]

let kind_to_trace_name = function
  | Created -> "create"
  | Deleted -> "delete"
  | File_changed -> "changed"
  | Unknown -> "unknown"
;;

let kind_repr =
  Repr.variant
    "file-watcher-event-kind"
    [ Repr.case0 "Created" ~test:(function
        | Created -> true
        | Deleted | File_changed | Unknown -> false)
    ; Repr.case0 "Deleted" ~test:(function
        | Deleted -> true
        | Created | File_changed | Unknown -> false)
    ; Repr.case0 "File_changed" ~test:(function
        | File_changed -> true
        | Created | Deleted | Unknown -> false)
    ; Repr.case0 "Unknown" ~test:(function
        | Unknown -> true
        | Created | Deleted | File_changed -> false)
    ]
;;

let to_event event =
  (* CR-soon rgrinberg: this timestamp is wrong *)
  let now = Time.now () in
  let name, args =
    match event with
    | `Queue_overflow -> "queue_overflow", []
    | `Sync id -> "sync", [ "id", Event.Arg.int id ]
    | `Watcher_terminated -> "watcher_terminated", []
    | `File (path, kind) -> kind_to_trace_name kind, [ "path", Event.Arg.path path ]
  in
  Event.Event.instant ~name ~args now File_watcher
;;
